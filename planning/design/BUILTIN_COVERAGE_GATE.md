# Forma 0.2 Builtin Coverage Gate

Forma 0.2 requires 100% test evidence for the authoritative builtin inventory in
`docs/builtins.json`. The gate is implemented by
`scripts/builtin_coverage.py` and exposed through
`scripts/builtin_coverage.sh`.

## What counts

A builtin has behavior/error evidence when an executed Rust test calls
`Interpreter::call_builtin` with its literal name, an executed
`builtin_behavior!` case names it, or an executed `.forma` integration/showcase
program calls it. Comments and unrelated identifier mentions do not count.

The gate reports four dimensions:

1. behavior or deterministic error-path evidence for every registered builtin;
2. deny/grant capability policy for every capability-gated builtin;
3. owned-parameter metadata and arity consistency;
4. interpreter, native, and verification support metadata for every builtin.

Successful native or verification execution is required only when that backend
claims support. An `Unsupported` claim must remain explicit and is covered as a
support-boundary assertion rather than being treated as successful execution.

## Adding a builtin

1. Add its registry and type metadata.
2. Add interpreter dispatch or mark the backend unsupported before registration.
3. Add a deterministic success case where practical.
4. Add invalid-input, error, capability, and ownership cases appropriate to the
   signature.
5. Update native and verification support claims.
6. Regenerate language metadata and run:

   ```bash
   forma grammar --write
   python3 scripts/generate_builtin_reference.py
   bash scripts/builtin_coverage.sh --enforce 100
   bash scripts/check_docs.sh
   ```

CI does not accept a reduced threshold. Temporary exemptions must be represented
as an explicit unsupported backend claim; interpreter-registered builtins receive
no behavior-coverage exemption.
