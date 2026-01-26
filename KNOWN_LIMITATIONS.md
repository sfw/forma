# FORMA v1.0 Known Limitations

This document lists known limitations in FORMA v1.0. These are tracked for future releases.

## Language Features

### Struct Update Syntax
The struct update syntax `{ ..base, field: value }` is parsed but not fully implemented in MIR lowering.
- **Workaround:** Create a new struct with all fields specified.

### Loop Labels
Loop labels for `break` and `continue` are parsed but not implemented.
- **Workaround:** Restructure code to avoid needing labeled breaks.

### Indirect Closure Calls
LLVM codegen doesn't support indirect calls for closures stored in variables.
- **Workaround:** Use direct function calls or interpreter mode.

## Tooling

### LSP Go-to-Definition
The `textDocument/definition` LSP method returns empty results.
- **Status:** Placeholder implementation, will be completed in v1.1.

### Formatter Completeness
The formatter handles most constructs but may output `?` for complex patterns.
- **Workaround:** Review formatted output before committing.

## Standard Library

### Async is Synchronous
The `sp` (spawn) and `aw` (await) keywords work but execute synchronously.
True parallelism is not implemented.
- **Note:** Useful for structuring async code, actual parallelism in v1.1.

## Type System

### Trait Bound Checking
Generic functions with trait bounds parse but bounds aren't fully checked at compile time.
- **Workaround:** Rely on runtime errors for trait method resolution.

### Higher-Kinded Types
Higher-kinded types (type constructors as parameters) are not supported.
- **Status:** Planned for v2.0.

## Parser

### Multiline Expressions
Some multiline expressions with trailing operators require parentheses:
```forma
# May fail:
a &&
    b

# Works:
(a) && (b)
# Or on single line:
a && b
```

---

*These limitations are tracked and planned for future releases.*
