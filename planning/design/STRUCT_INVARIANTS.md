# Struct Invariants

## Decision

Forma supports `@inv(condition[, message])` on struct declarations.

```forma
@inv(next_sequence == events.len(), "event sequence must be contiguous")
@inv(logical_time >= 0, "logical time must be non-negative")
s WorkflowState
    events: [AuditEvent]
    next_sequence: Int
    logical_time: Int
```

An invariant describes every valid externally observable instance of the
struct. It complements function contracts:

- `@pre` is a caller obligation.
- `@post` is a function guarantee.
- `@inv` is a type validity rule.

## 0.2 Semantics

Invariants are checked at these runtime boundaries:

1. immediately after a struct literal is initialized;
2. when an invariant-bearing value enters a function;
3. when an invariant-bearing value is returned from a function;
4. when a `ref mut` parameter is released back to its caller.

A function may temporarily violate an invariant while updating related fields,
provided it re-establishes the invariant before returning. Shared references
may observe only valid values. Affine ownership makes these boundaries
unambiguous.

Invariant expressions use the same pure contract expression subset as `@pre`
and `@post`. Struct fields are in scope by name. `result` and `old(...)` are not
available because invariants describe one value rather than a transition.

## Construction and external data

Ordinary struct literals are checked construction boundaries. Parsing JSON,
TOML, database rows, or network responses does not magically produce a valid
invariant-bearing value. Application decoders must validate untrusted data and
return an error before constructing the type.

An invariant failure is a programmer defect and produces a structured runtime
diagnostic naming the struct, boundary, condition, and optional message.
Expected invalid input should continue to use a fallible constructor or decoder.

## Verification

Runtime enforcement and proof are separate claims. The verifier counts
invariants as obligations at relevant boundaries. It reports `PROVED` only
inside its supported SMT subset and otherwise reports `UNKNOWN`; runtime checks
are never relabeled as formal proof.

## Future extensions

- opaque structs with module-private literals;
- explicitly fallible invariant construction;
- recursive invariants over nested invariant-bearing values;
- invariant-aware database and serialization derives;
- broader SMT support for structs, strings, and quantified collections.
