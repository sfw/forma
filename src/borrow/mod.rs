//! Borrow checker for ARIA.
//!
//! This module implements second-class reference checking, which ensures
//! memory safety without explicit lifetime annotations.
//!
//! # Second-Class References
//!
//! In ARIA, references are "second-class" - they cannot be stored in data
//! structures. This eliminates the need for lifetime annotations while
//! maintaining memory safety.
//!
//! ## What's Allowed
//!
//! - References as function parameters
//! - References as local variables
//! - Returning references derived from input references
//!
//! ## What's NOT Allowed
//!
//! - Storing references in structs
//! - Storing references in collections
//! - Returning references to local variables
//!
//! # Borrowing Rules
//!
//! - Multiple immutable borrows are allowed
//! - Only one mutable borrow at a time
//! - Cannot mix immutable and mutable borrows
//! - Cannot move a value while it's borrowed
//!
//! # Usage
//!
//! ```ignore
//! use aria::borrow::BorrowChecker;
//!
//! let mut checker = BorrowChecker::new();
//! let result = checker.check(&ast)?;
//! ```

pub mod checker;

pub use checker::{BorrowChecker, BorrowError, BorrowErrorKind, VarState};
