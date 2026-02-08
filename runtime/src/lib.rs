//! FORMA Runtime Library
//!
//! This library provides the runtime functions needed by LLVM-compiled FORMA programs.
//! All functions use the C ABI for compatibility with the generated LLVM IR.

#![allow(clippy::missing_safety_doc)]
#![allow(clippy::not_unsafe_ptr_arg_deref)] // FFI exports intentionally take raw pointers

pub mod env;
pub mod io;
pub mod map;
pub mod math;
pub mod memory;
pub mod panic;
pub mod string;
pub mod time;
pub mod vec;

// Re-export all public functions at the crate root for convenience
pub use env::*;
pub use io::*;
pub use map::*;
pub use math::*;
pub use memory::*;
pub use panic::*;
pub use string::*;
pub use time::*;
pub use vec::*;
