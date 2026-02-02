# Sprint 23: Fix Enum Variant Pattern Binding Types

**Goal:** Fix type inference for variables bound in enum variant patterns like `Ok(rows)` and `Some(x)`
**Priority:** HIGH - Blocks `forma check examples/cli_with_db.forma`

---

## Problem Analysis

### Symptom

`forma check examples/cli_with_db.forma` fails because when matching:

```forma
m db_query(db, "SELECT ...")
    Ok(rows) ->
        for row in rows     // ERROR: row type is wrong
            id := row_get_int(row, 0)
```

The variable `rows` gets bound to a type variable instead of `List[DbRow]`.

### Root Cause

**File:** `src/types/inference.rs` lines 4852-4864

In `collect_pattern_bindings`, when handling enum variant patterns:

```rust
Some((variant_name, TypeDef::Enum { variants, .. })) => {
    if let Some((_, field_types)) = variants.iter().find(|(n, _)| n == &variant_name) {
        for (field, expected_ty) in fields.iter().zip(field_types.iter()) {
            // BUG: expected_ty is Ty::Var(RESULT_T), not the actual type!
            env.insert(field.name.name.clone(), TypeScheme::mono(expected_ty.clone()));
        }
    }
}
```

The `field_types` from the enum definition contain **reserved type variables** like `Ty::Var(RESULT_T)`, not the actual concrete types.

For example, Result is registered as:
```rust
TypeDef::Enum {
    type_params: vec!["T", "E"],
    variants: vec![
        ("Ok", vec![Ty::Var(RESULT_T)]),    // T is a type variable!
        ("Err", vec![Ty::Var(RESULT_E)]),   // E is a type variable!
    ],
}
```

When matching `Ok(rows)` against `Result[List[DbRow], Str]`, we need to:
1. Know that we're matching against `Result[List[DbRow], Str]`
2. Map `RESULT_T` → `List[DbRow]` and `RESULT_E` → `Str`
3. Substitute `RESULT_T` in the variant's field types to get `List[DbRow]`
4. Bind `rows` to `List[DbRow]`

Currently step 3-4 don't happen - `rows` is bound to the raw `Ty::Var(RESULT_T)`.

---

## Task 23.1: Fix collect_pattern_bindings for Enum Variants

### Location
**File:** `src/types/inference.rs` around line 4852

### Solution

The `ty` parameter passed to `collect_pattern_bindings` IS the concrete type being matched (e.g., `Result[List[DbRow], Str]`). We need to:

1. Extract the type arguments from `ty` (e.g., `[List[DbRow], Str]`)
2. Map the enum's type parameters to these concrete types
3. Substitute when binding fields

### Fixed Code

Replace the enum branch in `collect_pattern_bindings` (around lines 4852-4864):

```rust
Some((variant_name, TypeDef::Enum { type_params, variants })) => {
    // Handle enum variant pattern (e.g., Some(x), Ok(value))
    if let Some((_, field_types)) = variants.iter().find(|(n, _)| n == &variant_name) {
        // Get concrete type arguments from the matched type
        // ty should be something like Result[List[DbRow], Str] or Option[Int]
        let resolved_ty = ty.apply(&self.unifier.subst);

        // Extract type arguments from the concrete type
        let type_args: Vec<Ty> = match &resolved_ty {
            Ty::Option(inner) => vec![(**inner).clone()],
            Ty::Result(ok_ty, err_ty) => vec![(**ok_ty).clone(), (**err_ty).clone()],
            Ty::Named(_, args) => args.clone(),
            _ => vec![],
        };

        // Build substitution map: type_param -> concrete_type
        // e.g., for Result: T -> List[DbRow], E -> Str
        let param_map: std::collections::HashMap<u32, Ty> = type_params.iter()
            .enumerate()
            .filter_map(|(i, param_name)| {
                // Map parameter name to reserved type var ID
                let var_id = match param_name.as_str() {
                    "T" if variant_name == "Some" || variant_name == "None" =>
                        Some(crate::types::inference::reserved_type_vars::OPTION_T),
                    "T" if variant_name == "Ok" || variant_name == "Err" =>
                        Some(crate::types::inference::reserved_type_vars::RESULT_T),
                    "E" if variant_name == "Ok" || variant_name == "Err" =>
                        Some(crate::types::inference::reserved_type_vars::RESULT_E),
                    _ => None,
                };

                var_id.and_then(|id| type_args.get(i).map(|ty| (id, ty.clone())))
            })
            .collect();

        // Bind each field pattern to its SUBSTITUTED type
        for (field, expected_ty) in fields.iter().zip(field_types.iter()) {
            // Substitute type variables with concrete types
            let concrete_ty = substitute_type_vars(expected_ty, &param_map);

            if let Some(p) = &field.pattern {
                self.collect_pattern_bindings(p, &concrete_ty, env)?;
            } else {
                // The field name is the binding
                env.insert(field.name.name.clone(), TypeScheme::mono(concrete_ty));
            }
        }
    }
}
```

### Helper Function

Add this helper function (can be added near the top of the file or as a method):

```rust
/// Substitute type variables with concrete types
fn substitute_type_vars(ty: &Ty, subst: &std::collections::HashMap<u32, Ty>) -> Ty {
    match ty {
        Ty::Var(tv) => {
            subst.get(&tv.id).cloned().unwrap_or_else(|| ty.clone())
        }
        Ty::List(inner) => Ty::List(Box::new(substitute_type_vars(inner, subst))),
        Ty::Option(inner) => Ty::Option(Box::new(substitute_type_vars(inner, subst))),
        Ty::Result(ok, err) => Ty::Result(
            Box::new(substitute_type_vars(ok, subst)),
            Box::new(substitute_type_vars(err, subst)),
        ),
        Ty::Tuple(elems) => Ty::Tuple(elems.iter().map(|t| substitute_type_vars(t, subst)).collect()),
        Ty::Fn(params, ret) => Ty::Fn(
            params.iter().map(|t| substitute_type_vars(t, subst)).collect(),
            Box::new(substitute_type_vars(ret, subst)),
        ),
        Ty::Ref(inner, m) => Ty::Ref(Box::new(substitute_type_vars(inner, subst)), *m),
        Ty::Named(id, args) => Ty::Named(
            id.clone(),
            args.iter().map(|t| substitute_type_vars(t, subst)).collect(),
        ),
        // Primitive types don't contain type variables
        _ => ty.clone(),
    }
}
```

---

## Task 23.2: Simpler Alternative - Use Applied Type Directly

An alternative, simpler approach: The concrete type is already available - we just need to extract field types from it directly, rather than from the enum definition.

### Alternative Solution

```rust
Some((variant_name, TypeDef::Enum { .. })) => {
    // Get the concrete type being matched
    let resolved_ty = ty.apply(&self.unifier.subst);

    // Extract field types directly from the concrete type
    let field_types: Vec<Ty> = match (&resolved_ty, variant_name.as_str()) {
        (Ty::Option(inner), "Some") => vec![(**inner).clone()],
        (Ty::Option(_), "None") => vec![],
        (Ty::Result(ok_ty, _), "Ok") => vec![(**ok_ty).clone()],
        (Ty::Result(_, err_ty), "Err") => vec![(**err_ty).clone()],
        // For user-defined enums, fall back to the definition
        // (would need more sophisticated handling)
        _ => {
            // Get from enum definition (existing behavior)
            if let Some(def) = self.env.get_type(&variant_name) {
                if let TypeDef::Enum { variants, .. } = def {
                    variants.iter()
                        .find(|(n, _)| n == &variant_name)
                        .map(|(_, tys)| tys.clone())
                        .unwrap_or_default()
                } else {
                    vec![]
                }
            } else {
                vec![]
            }
        }
    };

    // Bind each field pattern to its concrete type
    for (field, concrete_ty) in fields.iter().zip(field_types.iter()) {
        if let Some(p) = &field.pattern {
            self.collect_pattern_bindings(p, concrete_ty, env)?;
        } else {
            env.insert(field.name.name.clone(), TypeScheme::mono(concrete_ty.clone()));
        }
    }
}
```

This approach is simpler for the built-in Option/Result types.

---

## Task 23.3: Verify Fix

After implementation, run:

```bash
cargo test
cargo build --release
./target/release/forma check examples/cli_with_db.forma
./target/release/forma check examples/*.forma
```

All 13 examples should pass.

---

## Summary

| Task | Description | Priority |
|------|-------------|----------|
| 23.1 | Fix collect_pattern_bindings type substitution | HIGH |
| 23.2 | Alternative: Direct type extraction | MEDIUM |
| 23.3 | Verify all examples pass | HIGH |

**Root Issue:** Enum variant bindings use raw type variables from the definition instead of substituting with concrete types from the match context.

**Fix:** Extract concrete type arguments from the matched type and substitute them into field types before binding.

---

*"Types should flow, not stagnate."*
