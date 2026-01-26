# Sprint 21: Fix Option/Result Pattern Matching

**Goal:** Fix type checker to properly recognize Option/Result variant patterns
**Priority:** HIGH - Blocks `forma check std/*.forma` and `forma check examples/*.forma`

---

## Problem Analysis

When matching against Option/Result patterns like:

```forma
match result
    Ok(value) => ...
    Err(e) => ...
```

The type checker produces errors:
```
Unknown struct type in pattern: 'Ok'
Unknown struct type in pattern: 'Some'
```

### Root Cause

**File:** `src/types/inference.rs` lines 4580-4587

```rust
PatternKind::Struct(path, fields, rest) => {
    // Get the struct name from the path
    let struct_name = path.segments.last()
        .map(|s| s.name.name.clone())
        .unwrap_or_default();  // For "Some(x)", this is "Some"

    // Look up the struct definition
    let struct_def = self.env.get_type(&struct_name).cloned();  // Looks for type "Some"!
```

The issue is that for variant patterns like `Some(x)` or `Ok(value)`:
- `path.segments` contains only `["Some"]` or `["Ok"]`
- Code looks up "Some" as a type name
- But Option is registered under "Option", not "Some"
- The variant name "Some" is inside the TypeDef::Enum variants list

The code works for qualified patterns like `Option::Some(x)` (path has 2 segments) but not for unqualified variants.

---

## Task 21.1: Add Variant-to-Enum Resolution

### Solution

Add a lookup table mapping variant names to their parent enum types. When we see a single-segment path that's not a known type, check if it's a variant name and get the parent enum.

**File:** `src/types/inference.rs`

### Step 1: Add variant_to_enum map in TypeEnv

**Location:** Around line 2475 (in TypeEnv struct definition)

```rust
pub struct TypeEnv {
    bindings: HashMap<String, TypeScheme>,
    types: HashMap<String, TypeDef>,
    fn_info: HashMap<String, FunctionInfo>,
    traits: HashMap<String, TraitInfo>,
    // ADD THIS:
    /// Maps variant names to their parent enum name
    variant_to_enum: HashMap<String, String>,
}
```

### Step 2: Initialize in new() and with_builtins()

**Location:** Around line 2481 (TypeEnv::new)

```rust
pub fn new() -> Self {
    Self {
        bindings: HashMap::new(),
        types: HashMap::new(),
        fn_info: HashMap::new(),
        traits: HashMap::new(),
        variant_to_enum: HashMap::new(),  // ADD THIS
    }
}
```

**Location:** Around line 118 (with_builtins)

After adding Option and Result types, add variant mappings:

```rust
// Add Option type (existing code at line 121-130)
env.types.insert("Option".to_string(), TypeDef::Enum { ... });

// ADD after Option type registration:
env.variant_to_enum.insert("Some".to_string(), "Option".to_string());
env.variant_to_enum.insert("None".to_string(), "Option".to_string());

// Add Result type (existing code at line 152-162)
env.types.insert("Result".to_string(), TypeDef::Enum { ... });

// ADD after Result type registration:
env.variant_to_enum.insert("Ok".to_string(), "Result".to_string());
env.variant_to_enum.insert("Err".to_string(), "Result".to_string());
```

### Step 3: Update child() to clone variant_to_enum

**Location:** Around line 2544 (child method)

```rust
pub fn child(&self) -> Self {
    Self {
        bindings: self.bindings.clone(),
        types: self.types.clone(),
        fn_info: self.fn_info.clone(),
        traits: self.traits.clone(),
        variant_to_enum: self.variant_to_enum.clone(),  // ADD THIS
    }
}
```

### Step 4: Add insert_type that auto-registers variants

**Location:** After insert_type (around line 2513)

```rust
/// Insert a type definition, auto-registering enum variants.
pub fn insert_type(&mut self, name: String, def: TypeDef) {
    // If it's an enum, register all variant names
    if let TypeDef::Enum { variants, .. } = &def {
        for (variant_name, _) in variants {
            self.variant_to_enum.insert(variant_name.clone(), name.clone());
        }
    }
    self.types.insert(name, def);
}
```

### Step 5: Add helper to resolve variant to enum

**Location:** After get_type (around line 2519)

```rust
/// Look up an enum type by variant name.
pub fn get_enum_for_variant(&self, variant_name: &str) -> Option<(&str, &TypeDef)> {
    if let Some(enum_name) = self.variant_to_enum.get(variant_name) {
        if let Some(def) = self.types.get(enum_name) {
            return Some((enum_name, def));
        }
    }
    None
}
```

---

## Task 21.2: Update check_pattern for Variant Resolution

### Location
**File:** `src/types/inference.rs` around line 4580

### Current Code (BROKEN)

```rust
PatternKind::Struct(path, fields, rest) => {
    let struct_name = path.segments.last()
        .map(|s| s.name.name.clone())
        .unwrap_or_default();

    let struct_def = self.env.get_type(&struct_name).cloned();

    match struct_def {
        Some(TypeDef::Struct { ... }) => { ... }
        Some(TypeDef::Enum { ... }) => { ... }
        _ => {
            Err(TypeError::new(
                format!("Unknown struct type in pattern: '{}'", struct_name),
                pattern.span,
            ))
        }
    }
}
```

### Fixed Code

```rust
PatternKind::Struct(path, fields, rest) => {
    // Get the struct/variant name from the path
    let name = path.segments.last()
        .map(|s| s.name.name.clone())
        .unwrap_or_default();

    // Determine if this is a qualified path (Enum::Variant) or just a variant name
    let (struct_name, variant_name) = if path.segments.len() >= 2 {
        // Qualified: Option::Some, Result::Ok, Color::Red
        let enum_name = path.segments.first()
            .map(|s| s.name.name.clone())
            .unwrap_or_default();
        let var_name = path.segments.last()
            .map(|s| s.name.name.clone())
            .unwrap_or_default();
        (enum_name, Some(var_name))
    } else {
        // Unqualified: could be struct name OR variant name
        // First try as a type name
        if self.env.get_type(&name).is_some() {
            (name.clone(), None)
        } else if let Some((enum_name, _)) = self.env.get_enum_for_variant(&name) {
            // It's a variant name - resolve to parent enum
            (enum_name.to_string(), Some(name.clone()))
        } else {
            // Unknown - will produce error below
            (name.clone(), None)
        }
    };

    // Look up the struct/enum definition
    let struct_def = self.env.get_type(&struct_name).cloned();

    match struct_def {
        Some(TypeDef::Struct { type_params, fields: struct_fields }) => {
            // Struct pattern: Point { x, y }
            // (existing struct handling code - keep as is)

            // ... existing code ...
        }
        Some(TypeDef::Enum { type_params, variants }) => {
            // Enum variant pattern: Some(x), None, Color::Red
            let enum_ty = if type_params.is_empty() {
                Ty::Named(TypeId::new(&struct_name), vec![])
            } else {
                let type_args: Vec<Ty> = type_params.iter()
                    .map(|_| Ty::fresh_var())
                    .collect();
                Ty::Named(TypeId::new(&struct_name), type_args)
            };
            self.unifier.unify(&enum_ty, ty, pattern.span)?;

            // Get variant name - either from explicit variant_name or from fields
            let var_name = variant_name.unwrap_or_else(|| {
                fields.first()
                    .map(|f| f.name.name.clone())
                    .unwrap_or_default()
            });

            // Find variant in enum definition
            let variant = variants.iter()
                .find(|(vn, _)| vn == &var_name);

            if let Some((_, field_types)) = variant {
                // Validate field count
                let pattern_field_count = fields.len();
                if pattern_field_count != field_types.len() {
                    return Err(TypeError::new(
                        format!(
                            "variant '{}::{}' has {} field(s), pattern has {}",
                            struct_name, var_name, field_types.len(), pattern_field_count
                        ),
                        pattern.span,
                    ));
                }

                // Validate field types
                for (field, expected_ty) in fields.iter().zip(field_types.iter()) {
                    if let Some(nested_pat) = &field.pattern {
                        self.check_pattern(nested_pat, expected_ty)?;
                    }
                }
                Ok(())
            } else {
                let available: Vec<_> = variants.iter()
                    .map(|(name, _)| name.as_str())
                    .collect();
                Err(TypeError::new(
                    format!(
                        "enum '{}' has no variant '{}'. Available: {}",
                        struct_name, var_name, available.join(", ")
                    ),
                    pattern.span,
                ))
            }
        }
        _ => {
            // Unknown type - provide helpful error
            Err(TypeError::new(
                format!("Unknown type in pattern: '{}'. Not a known struct, enum, or variant.", name),
                pattern.span,
            ))
        }
    }
}
```

---

## Task 21.3: Update collect_pattern_bindings

Similar fix needed in `collect_pattern_bindings` which also has the same issue.

### Location
**File:** `src/types/inference.rs` around line 4770

Apply the same variant resolution logic to `collect_pattern_bindings`.

---

## Task 21.4: Register User-Defined Enum Variants

When the type checker processes user-defined enums, ensure variants are registered.

### Location
**File:** `src/types/inference.rs` around line 3477 (infer_item for Enum)

### Current Code

```rust
ItemKind::Enum(e) => {
    let type_params = self.get_type_params(&e.generics);
    // ... build variants ...
    self.env.insert_type(
        e.name.name.clone(),
        TypeDef::Enum {
            type_params: type_params.clone(),
            variants,
        },
    );
```

The `insert_type` method we updated in Task 21.1 Step 4 will automatically register variants when any enum is inserted. No additional changes needed here IF Step 4 is implemented correctly.

---

## Verification

After implementation, run:

```bash
cargo test
forma check std/*.forma
forma check examples/*.forma
```

All should pass without "Unknown struct type in pattern" errors.

---

## Summary

| Task | Description | Priority |
|------|-------------|----------|
| 21.1 | Add variant-to-enum resolution infrastructure | HIGH |
| 21.2 | Update check_pattern for variant lookup | HIGH |
| 21.3 | Update collect_pattern_bindings | HIGH |
| 21.4 | Auto-register user enum variants | MEDIUM |

**Expected outcome:** Pattern matching on Option, Result, and all user-defined enums works correctly with unqualified variant names (e.g., `Some(x)` instead of requiring `Option::Some(x)`).

---

*"Make the common case work."*
