# FORMA Memory Safety & FFI Design

**Date:** January 26, 2026
**Status:** Design Proposal for v2.0
**Goal:** True memory safety with safe FFI

---

## Executive Summary

FORMA currently has a **partial borrow checker** that provides compile-time warnings but has significant gaps:

| Feature | Current Status | Impact |
|---------|----------------|--------|
| Move tracking | ✅ Compile-time | Good |
| Borrow exclusivity | ✅ Basic | Good |
| Second-class refs | ✅ Enforced | Restrictive but safe |
| **Runtime enforcement** | ❌ Missing | **Critical gap** |
| **Lifetimes** | ❌ None | By design |
| **FFI safety** | ❌ Completely unsafe | **Critical gap** |

### Key Finding
References are implemented as **boxed copies** at runtime, not actual pointers. Mutation through `&mut` doesn't affect the original value. This is a fundamental semantic gap.

---

## Current Borrow Checker Analysis

### What Works
1. **Use-after-move detection** - Compile-time tracking of moved values
2. **Borrow exclusivity** - Cannot have `&` and `&mut` simultaneously
3. **Second-class references** - Refs cannot be stored in structs/collections
4. **Return safety** - Returned refs must derive from inputs

### Critical Gaps

| Gap | Line | Description |
|-----|------|-------------|
| No runtime enforcement | interp.rs:6544-6556 | `Move`, `Copy`, `Local` all clone |
| Refs are copies | interp.rs:6354-6361 | `&x` creates a boxed copy, not a pointer |
| No lifetime tracking | checker.rs | Borrows only end at scope exit |
| No NLL | checker.rs:983-989 | Purely lexical borrow scopes |
| No partial borrows | checker.rs:871-882 | Can't borrow different struct fields |
| No closure capture analysis | checker.rs:550-568 | No capture mode tracking |
| No async safety | checker.rs:632-638 | Can send refs across tasks |

---

## Proposed Solution: Linear Types + Capabilities

Based on research of Rust, Austral, and other systems, the recommended approach for FORMA v2.0:

### 1. Linear/Affine Type System

Linear types provide simple, mechanical rules that AI can easily follow:

```forma
# Linear type: must be used exactly once
linear s FileHandle
    fd: Int

# Creating a linear value
f open_file(path: Str) -> FileHandle
    fd := c_open(path, 0)
    FileHandle { fd }

# Consuming a linear value (only way to "use" it)
f close_file(handle: FileHandle) -> Unit
    c_close(handle.fd)
    # handle is consumed here - cannot be used again

# COMPILE ERROR: FileHandle not consumed
f leak_example()
    h := open_file("test.txt")
    # ERROR: linear value 'h' must be consumed before scope exit

# COMPILE ERROR: FileHandle used twice
f double_use_example()
    h := open_file("test.txt")
    close_file(h)
    close_file(h)  # ERROR: 'h' already consumed
```

**Benefits:**
- Prevents memory leaks (must consume resources)
- Prevents double-free (can only consume once)
- Prevents use-after-free (cannot use after consume)
- Simple rule: "use exactly once"
- Austral's linearity checker is only 600 lines

### 2. Capability-Based Security for FFI

Capabilities make permissions explicit and trackable:

```forma
# Capability types (built-in, unforgeable)
t FileCapability
t NetworkCapability
t ProcessCapability

# Functions that need capabilities must declare them
f read_config(cap: &FileCapability, path: Str) -> Str
    # Can only read files because we have FileCapability
    file_read(path)

# Main receives root capabilities from runtime
f main(fs: FileCapability, net: NetworkCapability)
    # Can pass capabilities to functions that need them
    config := read_config(&fs, "config.json")

    # Cannot call network functions without passing net capability
    # fetch_url("http://example.com")  # ERROR: missing NetworkCapability

# Supply chain safety: imported modules can only do what you permit
us untrusted_lib

f safe_computation()
    # untrusted_lib cannot access filesystem or network
    # because we didn't pass it any capabilities
    result := untrusted_lib.compute(42)
```

**Benefits:**
- Prevents supply chain attacks
- AI can track capability flow
- Principle of least privilege
- No ambient authority

### 3. Module-Level Unsafe Isolation

Separate safe and unsafe code:

```forma
# Regular module - cannot use FFI
md SafeMath
    f add(a: Int, b: Int) -> Int
        a + b

# Unsafe module - can use FFI, requires explicit marking
unsafe md FFI
    # External C functions
    extern f c_malloc(size: Int) -> RawPtr
    extern f c_free(ptr: RawPtr)
    extern f c_open(path: CStr, flags: Int) -> Int

    # Safe wrappers using linear types
    linear s ManagedPtr
        ptr: RawPtr
        size: Int

    f allocate(size: Int) -> ManagedPtr
        ptr := c_malloc(size)
        ManagedPtr { ptr, size }

    f deallocate(mem: ManagedPtr)
        c_free(mem.ptr)
        # mem is consumed

# Using unsafe module from safe code
us FFI

f example()
    # Must consume the linear ManagedPtr
    mem := FFI.allocate(1024)
    # ... use mem ...
    FFI.deallocate(mem)  # Required: consume the resource
```

**Benefits:**
- Clear separation of safe/unsafe code
- Easier auditing (only review unsafe modules)
- Unsafe code is contained
- Safe wrappers expose safe interfaces

---

## Implementation Plan

### Phase 1: Linear Types (v2.0-alpha)

**Changes Required:**

1. **Type System** (`src/types/`)
   - Add `LinearKind` enum: `Linear`, `Affine`, `Normal`
   - Track linearity in type definitions
   - Add `linear` keyword to parser

2. **Borrow Checker** (`src/borrow/checker.rs`)
   - Add consumption tracking for linear values
   - Error on unconsumed linear values at scope exit
   - Error on multiple uses of linear values
   - Add `Consumed` state to `VarState` enum

3. **MIR** (`src/mir/`)
   - Add `Consume` operation distinct from `Move`
   - Track linearity in MIR types
   - Validate linear usage in lowering

4. **Runtime** (`src/mir/interp.rs`)
   - Actually invalidate consumed values
   - Runtime check for double-consumption
   - Clear consumed locals from frame

### Phase 2: Capabilities (v2.0-beta)

**Changes Required:**

1. **Built-in Capability Types**
   - `FileCapability`, `NetworkCapability`, `ProcessCapability`, etc.
   - Capabilities are linear (cannot be duplicated)
   - Root capabilities provided to `main()`

2. **Capability Checking**
   - Track capability requirements in function signatures
   - Verify capability availability at call sites
   - Prevent capability forgery

3. **Module System**
   - Modules receive only capabilities passed to them
   - No ambient authority

### Phase 3: Unsafe Modules (v2.0)

**Changes Required:**

1. **Parser**
   - Add `unsafe md` syntax
   - Add `extern f` for foreign functions
   - Add `RawPtr`, `CStr` types

2. **Module System**
   - Track module safety level
   - Prevent FFI calls from safe modules
   - Require capability for unsafe module usage

3. **FFI System**
   - C function declarations
   - Type marshalling
   - Safe wrapper generation

---

## Comparison: Before and After

### Before (Current FORMA v1.x)

```forma
# Current: No safety, refs are copies, FFI is dangerous
f dangerous()
    ptr := alloc(1024)        # Raw pointer, no tracking
    # ... forget to free ...   # Memory leak, no error

f also_dangerous()
    ptr := alloc(1024)
    dealloc(ptr, 1024)
    dealloc(ptr, 1024)        # Double free, no error at compile time
```

### After (Proposed FORMA v2.0)

```forma
# After: Linear types prevent leaks and double-free
f safe()
    mem := allocate(1024)     # Linear type ManagedPtr
    # ... use mem ...
    deallocate(mem)           # Must consume
    # deallocate(mem)         # ERROR: already consumed

f also_safe()
    mem := allocate(1024)
    # function exit without deallocate(mem)
    # ERROR: linear value 'mem' not consumed
```

---

## Trade-offs

| Approach | Simplicity | Safety | Performance | AI-Friendliness |
|----------|------------|--------|-------------|-----------------|
| Current (none) | ✅ High | ❌ Low | ✅ High | ❌ Low |
| Rust-style | ⚠️ Medium | ✅ High | ✅ High | ⚠️ Medium |
| **Linear + Capabilities** | ✅ High | ✅ High | ✅ High | ✅ High |
| Full GC | ✅ High | ✅ High | ⚠️ Medium | ✅ High |

**Recommended: Linear Types + Capabilities**

- Simple rules AI can follow mechanically
- No lifetime annotations needed
- Compile-time guarantees
- No GC overhead
- Clear separation of safe/unsafe code

---

## Migration Path

### v1.x → v2.0

1. **v1.4**: Document current limitations clearly
2. **v1.5**: Add opt-in `linear` keyword (no enforcement yet)
3. **v2.0-alpha**: Enforce linearity for `linear` types
4. **v2.0-beta**: Add capabilities
5. **v2.0**: Full unsafe module system

### Backwards Compatibility

- Non-linear types continue to work as before
- FFI functions deprecated in favor of unsafe modules
- Gradual adoption path for existing code

---

## Summary

FORMA's memory safety can be fixed with a **linear type system** combined with **capability-based security** and **module-level unsafe isolation**. This approach:

1. **Fixes memory leaks** - Linear types must be consumed
2. **Fixes double-free** - Linear types can only be consumed once
3. **Fixes use-after-free** - Cannot use after consumption
4. **Fixes FFI safety** - Unsafe code isolated in marked modules
5. **Prevents supply chain attacks** - Capabilities control resource access
6. **AI-friendly** - Simple, mechanical rules

This is a significant change best targeted for **FORMA v2.0**.

---

*"Safety through simplicity."*
