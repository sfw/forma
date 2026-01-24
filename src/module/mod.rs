//! Module system for ARIA.
//!
//! This module provides functionality for loading and resolving
//! external modules from files.

mod loader;

pub use loader::{ModuleError, ModuleLoader, LoadedModule};
