# Known LLVM Codegen Issues

## Issue 1: Multiple Recursive Calls with Arithmetic Causes Segfault

**Status:** OPEN
**Severity:** HIGH
**Discovered:** Sprint 24 verification

### Reproduction

```forma
# examples/fibonacci.forma
f fib(n: Int) -> Int
    if n <= 1 then n else fib(n - 1) + fib(n - 2)

f main() -> Int = fib(10)
```

Compiling with `forma build examples/fibonacci.forma -o fib` and running `./fib` causes a segmentation fault.

### Analysis

The pattern `fib(n-1) + fib(n-2)` involves:
1. First recursive call `fib(n-1)` → stores result in local
2. Second recursive call `fib(n-2)` → stores result in local
3. Addition of both results

Simple recursion (single call) works. The issue appears when two function calls are combined with arithmetic in the same expression.

### Suspected Causes

1. **Local variable handling:** The call results may not be properly preserved across the second call
2. **Stack frame corruption:** Something in the calling convention is wrong
3. **Register allocation:** LLVM may be reusing registers that should be preserved

### Workaround

Use the interpreter (`forma run`) instead of LLVM compilation for recursive functions with multiple calls.

### To Investigate

1. Generate LLVM IR (`--emit-llvm`) and inspect the generated code
2. Compare with equivalent C code compiled by clang
3. Check if explicit local storage helps
4. Test with optimization disabled (`-O0`)

---

## Issue 2: No Builtin Functions in LLVM Backend

**Status:** IN PROGRESS (Sprint 24-R)
**Severity:** CRITICAL

### Description

LLVM codegen doesn't know about builtin functions like `print`, `vec_len`, etc. These are implemented in the interpreter but have no LLVM counterpart.

### Solution

Sprint 24-R creates a runtime library (`libforma_runtime.a`) that provides C ABI implementations of all builtins. LLVM codegen will emit calls to these external functions, and the linker will resolve them.

---

## Issue 3: Async Not Fully Supported

**Status:** DOCUMENTED
**Severity:** MEDIUM

### Description

`Terminator::Spawn` and `Terminator::Await` are implemented as synchronous calls in LLVM. True async support would require:
- Coroutine state machines
- Async runtime integration
- Stack switching or continuation passing

### Current Behavior

`sp expr` evaluates `expr` synchronously and returns immediately. `aw future` returns the already-computed value.

### Future Work

Consider integrating with `libuv` or implementing LLVM coroutines for true async support.
