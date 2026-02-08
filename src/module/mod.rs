//! Module system for FORMA.
//!
//! This module provides functionality for loading and resolving
//! external modules from files.

mod loader;

pub use loader::{LoadedModule, ModuleError, ModuleLoader};
