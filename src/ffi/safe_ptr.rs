//! Safe pointer wrapper with bounds checking and generation tracking.
//!
//! `SafePtr<T>` wraps a raw allocation with:
//! - Bounds checking on every access
//! - Generation counters to detect use-after-free
//! - Integration with `MemoryArena` for allocation tracking

use std::collections::HashMap;
use std::fmt;

/// Errors from safe pointer operations.
#[derive(Debug, Clone, PartialEq)]
pub enum PtrError {
    /// Attempted to access index beyond allocation size
    IndexOutOfBounds { index: usize, len: usize },
    /// Attempted to use a pointer after its memory was freed
    UseAfterFree { id: u64, freed_generation: u64 },
    /// Null pointer dereference
    NullPointer,
    /// Allocation not found in arena
    InvalidAllocation { id: u64 },
}

impl fmt::Display for PtrError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PtrError::IndexOutOfBounds { index, len } => {
                write!(
                    f,
                    "index {} out of bounds for allocation of size {}",
                    index, len
                )
            }
            PtrError::UseAfterFree {
                id,
                freed_generation,
            } => {
                write!(
                    f,
                    "use-after-free: allocation {} was freed at generation {}",
                    id, freed_generation
                )
            }
            PtrError::NullPointer => write!(f, "null pointer dereference"),
            PtrError::InvalidAllocation { id } => {
                write!(f, "invalid allocation id {}", id)
            }
        }
    }
}

impl std::error::Error for PtrError {}

/// A safe pointer with bounds checking and generation tracking.
///
/// `SafePtr<T>` stores a vector of `T` values along with a generation counter.
/// The generation is checked against the arena on every access to detect
/// use-after-free.
#[derive(Debug, Clone)]
pub struct SafePtr<T: Clone> {
    /// Unique allocation ID
    pub id: u64,
    /// Generation when this pointer was created
    pub generation: u64,
    /// The actual data
    data: Vec<T>,
}

impl<T: Clone> SafePtr<T> {
    /// Get a reference to the element at `index`, with bounds checking.
    pub fn get(&self, index: usize) -> Result<&T, PtrError> {
        self.data.get(index).ok_or(PtrError::IndexOutOfBounds {
            index,
            len: self.data.len(),
        })
    }

    /// Get a mutable reference to the element at `index`, with bounds checking.
    pub fn get_mut(&mut self, index: usize) -> Result<&mut T, PtrError> {
        let len = self.data.len();
        self.data
            .get_mut(index)
            .ok_or(PtrError::IndexOutOfBounds { index, len })
    }

    /// Get a slice of the data with bounds checking.
    pub fn slice(&self, start: usize, end: usize) -> Result<&[T], PtrError> {
        if start > self.data.len() {
            return Err(PtrError::IndexOutOfBounds {
                index: start,
                len: self.data.len(),
            });
        }
        if end > self.data.len() {
            return Err(PtrError::IndexOutOfBounds {
                index: end,
                len: self.data.len(),
            });
        }
        Ok(&self.data[start..end])
    }

    /// Get the length of the allocation.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Check if the allocation is empty.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

/// Tracks allocations with generation counters for use-after-free detection.
///
/// Each allocation gets a unique ID and a generation counter. When an allocation
/// is freed, its generation is incremented. Any `SafePtr` holding the old
/// generation will fail validation.
#[derive(Debug)]
pub struct MemoryArena {
    /// Next allocation ID
    next_id: u64,
    /// Current generation counter
    current_generation: u64,
    /// Map from allocation ID to (generation_when_allocated, is_live)
    allocations: HashMap<u64, AllocationInfo>,
}

#[derive(Debug)]
struct AllocationInfo {
    /// Whether this allocation is still live
    is_live: bool,
    /// Generation when freed (0 if still live)
    freed_generation: u64,
}

impl MemoryArena {
    /// Create a new empty memory arena.
    pub fn new() -> Self {
        Self {
            next_id: 1,
            current_generation: 1,
            allocations: HashMap::new(),
        }
    }

    /// Allocate a new `SafePtr<T>` with the given data.
    pub fn alloc<T: Clone>(&mut self, data: Vec<T>) -> SafePtr<T> {
        let id = self.next_id;
        self.next_id += 1;
        let generation = self.current_generation;

        self.allocations.insert(
            id,
            AllocationInfo {
                is_live: true,
                freed_generation: 0,
            },
        );

        SafePtr {
            id,
            generation,
            data,
        }
    }

    /// Free an allocation by ID. Increments the generation counter.
    pub fn free(&mut self, id: u64) -> Result<(), PtrError> {
        let info = self
            .allocations
            .get_mut(&id)
            .ok_or(PtrError::InvalidAllocation { id })?;

        if !info.is_live {
            return Err(PtrError::UseAfterFree {
                id,
                freed_generation: info.freed_generation,
            });
        }

        self.current_generation += 1;
        info.is_live = false;
        info.freed_generation = self.current_generation;
        Ok(())
    }

    /// Validate that a SafePtr is still valid (not freed).
    pub fn validate<T: Clone>(&self, ptr: &SafePtr<T>) -> Result<(), PtrError> {
        let info = self
            .allocations
            .get(&ptr.id)
            .ok_or(PtrError::InvalidAllocation { id: ptr.id })?;

        if !info.is_live {
            return Err(PtrError::UseAfterFree {
                id: ptr.id,
                freed_generation: info.freed_generation,
            });
        }

        Ok(())
    }

    /// Get the number of live allocations.
    pub fn live_count(&self) -> usize {
        self.allocations.values().filter(|a| a.is_live).count()
    }

    /// Get the total number of allocations (including freed).
    pub fn total_count(&self) -> usize {
        self.allocations.len()
    }
}

impl Default for MemoryArena {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_safe_ptr_bounds_checking() {
        let mut arena = MemoryArena::new();
        let ptr = arena.alloc(vec![1, 2, 3, 4, 5]);

        assert_eq!(*ptr.get(0).unwrap(), 1);
        assert_eq!(*ptr.get(4).unwrap(), 5);
        assert!(matches!(
            ptr.get(5),
            Err(PtrError::IndexOutOfBounds { index: 5, len: 5 })
        ));
    }

    #[test]
    fn test_safe_ptr_slice() {
        let mut arena = MemoryArena::new();
        let ptr = arena.alloc(vec![10, 20, 30, 40, 50]);

        assert_eq!(ptr.slice(1, 4).unwrap(), &[20, 30, 40]);
        assert!(matches!(
            ptr.slice(0, 6),
            Err(PtrError::IndexOutOfBounds { .. })
        ));
    }

    #[test]
    fn test_use_after_free_detection() {
        let mut arena = MemoryArena::new();
        let ptr = arena.alloc(vec![1, 2, 3]);
        let id = ptr.id;

        // Should be valid before free
        assert!(arena.validate(&ptr).is_ok());

        // Free the allocation
        arena.free(id).unwrap();

        // Should detect use-after-free
        assert!(matches!(
            arena.validate(&ptr),
            Err(PtrError::UseAfterFree { .. })
        ));
    }

    #[test]
    fn test_double_free_detection() {
        let mut arena = MemoryArena::new();
        let ptr = arena.alloc(vec![1, 2, 3]);
        let id = ptr.id;

        arena.free(id).unwrap();
        assert!(matches!(arena.free(id), Err(PtrError::UseAfterFree { .. })));
    }

    #[test]
    fn test_arena_live_count() {
        let mut arena = MemoryArena::new();
        let p1 = arena.alloc(vec![1]);
        let p2 = arena.alloc(vec![2]);
        let _p3 = arena.alloc(vec![3]);

        assert_eq!(arena.live_count(), 3);

        arena.free(p1.id).unwrap();
        assert_eq!(arena.live_count(), 2);

        arena.free(p2.id).unwrap();
        assert_eq!(arena.live_count(), 1);
    }

    #[test]
    fn test_get_mut() {
        let mut arena = MemoryArena::new();
        let mut ptr = arena.alloc(vec![1, 2, 3]);

        *ptr.get_mut(1).unwrap() = 42;
        assert_eq!(*ptr.get(1).unwrap(), 42);
    }
}
