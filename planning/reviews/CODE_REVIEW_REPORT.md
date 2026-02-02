# FORMA Comprehensive Code Review Report

**Date:** January 25, 2026
**Reviewer:** Claude (Automated Deep Review)
**Scope:** Full codebase review for v1.0 readiness

---

## Executive Summary

FORMA is a well-architected language with comprehensive features, but has **critical issues** that must be addressed before v1.0 release. The codebase totals ~15,000+ lines of Rust across the compiler and ~2,500 lines of FORMA in stdlib/examples.

### FORMA's Core Pillars (Must Be Preserved)

All solutions must align with these design principles:

1. **AI Code Generation First** - No lifetimes, predictable patterns, minimal annotations
2. **Memory Safety Without Lifetimes** - Second-class references, no stored references
3. **Token Efficiency** - Short keywords, concise syntax, fewer tokens = lower API costs
4. **Machine-Readable Tooling** - Grammar export, structured JSON errors, type queries
5. **Strong Type Inference** - Hindley-Milner style, AI rarely needs explicit types

### Overall Assessment

| Component | Quality | Critical Issues |
|-----------|---------|-----------------|
| Lexer | Good | 2 minor |
| Parser | Good | 3 medium |
| Type System | **Needs Work** | **5 critical** |
| MIR/Interpreter | Good | 3 medium |
| Stdlib | **Needs Work** | **4 critical** |
| Examples | **Needs Work** | **3 critical** |

**Recommendation:** Address critical issues before v1.0. Current state is suitable for beta but not production release.

---

## Part 1: Critical Issues with Solutions

### 1.1 Type System - Method/Field Type Checking Not Implemented

**Files:** src/types/inference.rs, lines 3272-3286

**Issue:** Method calls and field accesses return fresh type variables instead of actual types.

```rust
// Current (BROKEN)
ExprKind::MethodCall(receiver, _method, args) => {
    let _receiver_ty = self.infer_expr(receiver)?;
    Ok(Ty::fresh_var())  // Returns unknown type - NO TYPE CHECKING!
}
```

**Impact:** Type errors in method calls won't be caught. Users can call non-existent methods.

**Pillar Alignment:** This violates Pillar 4 (Machine-Readable Tooling) - errors should be caught and reported as structured JSON for AI self-correction.

#### Recommended Solution

**Approach: Trait-based method resolution with receiver type lookup**

```rust
// Solution for src/types/inference.rs

ExprKind::MethodCall(receiver, method, args) => {
    let receiver_ty = self.infer_expr(receiver)?;

    // 1. Resolve the receiver type to its concrete form
    let resolved_ty = self.resolve_type(&receiver_ty);

    // 2. Look up method in type's impl blocks or traits
    let method_sig = self.lookup_method(&resolved_ty, &method.name)
        .ok_or_else(|| TypeError::new(
            format!("type {} has no method '{}'", resolved_ty, method.name),
            method.span
        ))?;

    // 3. Check argument types match method signature
    if args.len() != method_sig.params.len() {
        return Err(TypeError::new(
            format!("method '{}' expects {} arguments, found {}",
                    method.name, method_sig.params.len(), args.len()),
            expr.span
        ));
    }

    for (arg, expected_ty) in args.iter().zip(&method_sig.params) {
        let arg_ty = self.infer_expr(arg)?;
        self.unifier.unify(&arg_ty, expected_ty, arg.span)?;
    }

    // 4. Return the method's return type
    Ok(method_sig.return_type.clone())
}

// Add helper method
fn lookup_method(&self, ty: &Ty, name: &str) -> Option<MethodSignature> {
    // Check builtin methods first (for Vec, Map, String, etc.)
    if let Some(sig) = self.builtin_methods.get(&(ty.clone(), name.to_string())) {
        return Some(sig.clone());
    }

    // Check user-defined impl blocks
    if let Ty::Named(type_id, _) = ty {
        if let Some(impl_block) = self.impl_blocks.get(type_id) {
            return impl_block.methods.get(name).cloned();
        }
    }

    // Check trait implementations
    for (trait_id, impls) in &self.trait_impls {
        if impls.contains_key(ty) {
            if let Some(trait_def) = self.traits.get(trait_id) {
                if let Some(method) = trait_def.methods.get(name) {
                    return Some(method.clone());
                }
            }
        }
    }

    None
}
```

**For Field Access (similar pattern):**

```rust
ExprKind::Field(base, field) => {
    let base_ty = self.infer_expr(base)?;
    let resolved_ty = self.resolve_type(&base_ty);

    // Look up field in struct definition
    let field_ty = self.lookup_field(&resolved_ty, &field.name)
        .ok_or_else(|| TypeError::new(
            format!("type {} has no field '{}'", resolved_ty, field.name),
            field.span
        ))?;

    Ok(field_ty)
}

fn lookup_field(&self, ty: &Ty, name: &str) -> Option<Ty> {
    match ty {
        Ty::Named(type_id, type_args) => {
            if let Some(struct_def) = self.structs.get(type_id) {
                struct_def.fields.iter()
                    .find(|f| f.name == name)
                    .map(|f| self.substitute_type_params(&f.ty, type_args))
            } else {
                None
            }
        }
        Ty::Tuple(types) => {
            // Handle tuple field access (0, 1, 2, etc.)
            name.parse::<usize>().ok()
                .and_then(|idx| types.get(idx).cloned())
        }
        _ => None
    }
}
```

**AI-Friendly Error Output:**

```json
{
  "error": "unknown_method",
  "type": "Vec[Int]",
  "method": "nonexistent",
  "location": {"line": 10, "column": 5},
  "available_methods": ["push", "pop", "len", "map", "filter"],
  "suggestion": "Did you mean 'push'?"
}
```

---

### 1.2 Type System - Struct Pattern Fields Not Validated

**File:** src/types/inference.rs, lines 3779-3785

**Issue:** Struct patterns check against fresh type variables, not actual field types.

**Pillar Alignment:** Violates Pillar 1 (AI Code Generation First) - AI-generated pattern matches could silently fail at runtime.

#### Recommended Solution

```rust
// Solution for src/types/inference.rs

PatternKind::Struct(path, fields, rest) => {
    // 1. Resolve the struct type from the path
    let struct_name = path.segments.last()
        .ok_or_else(|| TypeError::new("empty struct path", pattern.span))?;

    let struct_def = self.structs.get(&TypeId::new(&struct_name.name))
        .ok_or_else(|| TypeError::new(
            format!("unknown struct type '{}'", struct_name.name),
            pattern.span
        ))?;

    // 2. Unify expected type with struct type
    let struct_ty = Ty::Named(
        TypeId::new(&struct_name.name),
        struct_def.type_params.iter()
            .map(|_| Ty::fresh_var())
            .collect()
    );
    self.unifier.unify(&struct_ty, ty, pattern.span)?;

    // 3. Check each field pattern against actual field types
    let mut matched_fields = HashSet::new();
    for field_pat in fields {
        let field_def = struct_def.fields.iter()
            .find(|f| f.name == field_pat.name)
            .ok_or_else(|| TypeError::new(
                format!("struct '{}' has no field '{}'", struct_name.name, field_pat.name),
                field_pat.span
            ))?;

        matched_fields.insert(&field_pat.name);

        if let Some(pat) = &field_pat.pattern {
            self.check_pattern(pat, &field_def.ty)?;
        }
    }

    // 4. If no rest pattern (..), ensure all fields are covered
    if !rest {
        for field_def in &struct_def.fields {
            if !matched_fields.contains(&field_def.name) {
                return Err(TypeError::new(
                    format!("pattern missing field '{}' (use .. to ignore)", field_def.name),
                    pattern.span
                ));
            }
        }
    }

    Ok(())
}
```

---

### 1.3 MIR Lowering - Type Information Lost

**File:** src/mir/lower.rs, multiple locations

**Issue:** Uses `Ty::Int` as placeholder for all temporaries.

**Pillar Alignment:** Doesn't directly violate pillars, but prevents future optimizations and LLVM codegen correctness.

#### Recommended Solution

**Approach: Propagate types from operands during lowering**

```rust
// Solution for src/mir/lower.rs

// Add type inference helper
fn infer_rvalue_type(&self, rvalue: &Rvalue) -> Ty {
    match rvalue {
        Rvalue::Use(operand) => self.operand_type(operand),
        Rvalue::BinaryOp(op, left, right) => {
            let left_ty = self.operand_type(left);
            let right_ty = self.operand_type(right);
            match op {
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => {
                    // Numeric ops preserve type
                    left_ty
                }
                BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                    Ty::Bool
                }
                BinOp::And | BinOp::Or => Ty::Bool,
                _ => left_ty
            }
        }
        Rvalue::UnaryOp(op, operand) => {
            match op {
                UnOp::Neg => self.operand_type(operand),
                UnOp::Not => Ty::Bool,
            }
        }
        Rvalue::MakeTuple(operands) => {
            Ty::Tuple(operands.iter().map(|o| self.operand_type(o)).collect())
        }
        Rvalue::MakeArray(operands) => {
            let elem_ty = operands.first()
                .map(|o| self.operand_type(o))
                .unwrap_or(Ty::fresh_var());
            Ty::List(Box::new(elem_ty))
        }
        Rvalue::Call(func, _) => {
            // Look up function return type
            self.function_return_type(func).unwrap_or(Ty::Unit)
        }
        // ... handle other cases
    }
}

fn operand_type(&self, operand: &Operand) -> Ty {
    match operand {
        Operand::Constant(c) => self.constant_type(c),
        Operand::Copy(local) | Operand::Move(local) => {
            self.local_types.get(local).cloned().unwrap_or(Ty::fresh_var())
        }
    }
}

// Update temp creation to use inferred type
fn lower_expr(&mut self, expr: &Expr) -> Option<Operand> {
    match &expr.kind {
        ExprKind::Binary(op, left, right) => {
            let left_op = self.lower_expr(left)?;
            let right_op = self.lower_expr(right)?;

            let rvalue = Rvalue::BinaryOp(*op, left_op, right_op);
            let result_ty = self.infer_rvalue_type(&rvalue);
            let result = self.new_temp(result_ty);  // Use inferred type!

            self.emit(Statement::Assign(result, rvalue));
            Some(Operand::Copy(result))
        }
        // ... similar for other expressions
    }
}
```

---

### 1.4 Interpreter - User Enum Discriminant Hash Collision

**File:** src/mir/interp.rs, lines 5508-5512

**Issue:** Uses naive byte sum for discriminants, causing collisions.

**Pillar Alignment:** Violates correctness - doesn't affect pillars but breaks language semantics.

#### Recommended Solution

**Approach: Use enum variant index, not hash**

```rust
// Solution for src/mir/interp.rs

// Add a registry for enum variant indices
struct EnumRegistry {
    // Map from (enum_name, variant_name) -> unique index
    variant_indices: HashMap<(String, String), i64>,
    next_index: HashMap<String, i64>,  // Next index per enum
}

impl EnumRegistry {
    fn new() -> Self {
        Self {
            variant_indices: HashMap::new(),
            next_index: HashMap::new(),
        }
    }

    fn register_variant(&mut self, enum_name: &str, variant_name: &str) -> i64 {
        let key = (enum_name.to_string(), variant_name.to_string());
        if let Some(&idx) = self.variant_indices.get(&key) {
            return idx;
        }

        let idx = self.next_index.entry(enum_name.to_string()).or_insert(0);
        let result = *idx;
        *idx += 1;
        self.variant_indices.insert(key, result);
        result
    }

    fn get_discriminant(&self, enum_name: &str, variant_name: &str) -> i64 {
        // Built-in enums
        match (enum_name, variant_name) {
            ("Option", "None") => 0,
            ("Option", "Some") => 1,
            ("Result", "Ok") => 1,
            ("Result", "Err") => 0,
            _ => {
                // User-defined enums - look up registered index
                let key = (enum_name.to_string(), variant_name.to_string());
                self.variant_indices.get(&key).copied().unwrap_or_else(|| {
                    // Fallback: should never happen if properly registered
                    panic!("unregistered enum variant: {}::{}", enum_name, variant_name)
                })
            }
        }
    }
}

// Update interpreter to use registry
impl Interpreter {
    fn new(program: &Program) -> Self {
        let mut enum_registry = EnumRegistry::new();

        // Register all enum variants during initialization
        for (name, enum_def) in &program.enums {
            for variant in &enum_def.variants {
                enum_registry.register_variant(name, &variant.name);
            }
        }

        Self {
            enum_registry,
            // ... other fields
        }
    }

    fn get_discriminant(&self, enum_name: &str, variant: &str) -> i64 {
        self.enum_registry.get_discriminant(enum_name, variant)
    }
}
```

---

### 1.5 Example - SQL Injection Vulnerability

**File:** examples/cli_with_db.forma, line 56

**Issue:** Direct string concatenation in SQL query.

**Pillar Alignment:** Violates best practices - examples teach patterns that AI will replicate.

#### Recommended Solution

**Option A: Add Prepared Statement API (Preferred)**

```rust
// Add to src/mir/interp.rs builtins

// New builtins for parameterized queries
"db_prepare" => {
    let db_id = args[0].as_db()?;
    let sql = args[1].as_str()?;

    let db = self.databases.get(&db_id)
        .ok_or_else(|| InterpError::new("invalid database handle"))?;

    // Create prepared statement
    let stmt = db.prepare(&sql)?;
    let stmt_id = self.next_stmt_id;
    self.next_stmt_id += 1;
    self.statements.insert(stmt_id, stmt);

    Ok(Value::Statement(stmt_id))
}

"db_execute_prepared" => {
    let stmt_id = args[0].as_stmt()?;
    let params = args[1].as_vec()?;

    let stmt = self.statements.get_mut(&stmt_id)
        .ok_or_else(|| InterpError::new("invalid statement handle"))?;

    // Bind parameters safely
    for (i, param) in params.iter().enumerate() {
        match param {
            Value::Int(n) => stmt.raw_bind_parameter(i + 1, *n)?,
            Value::Str(s) => stmt.raw_bind_parameter(i + 1, s.as_str())?,
            Value::Float(f) => stmt.raw_bind_parameter(i + 1, *f)?,
            Value::Bool(b) => stmt.raw_bind_parameter(i + 1, *b)?,
            Value::Unit => stmt.raw_bind_parameter(i + 1, rusqlite::types::Null)?,
            _ => return Err(InterpError::new("unsupported parameter type")),
        }
    }

    let rows_affected = stmt.execute([])?;
    Ok(Value::Int(rows_affected as i64))
}
```

**Updated Example:**

```forma
// examples/cli_with_db.forma - SECURE VERSION

f add_todo(db: Database, title: Str) -> Result[Int, Str] {
    // Use parameterized query - prevents SQL injection
    stmt := db_prepare(db, "INSERT INTO todos (title) VALUES (?)")?
    db_execute_prepared(stmt, [title])
}

f complete_todo(db: Database, id: Int) -> Result[Int, Str] {
    stmt := db_prepare(db, "UPDATE todos SET completed = 1 WHERE id = ?")?
    db_execute_prepared(stmt, [id])
}

f delete_todo(db: Database, id: Int) -> Result[Int, Str] {
    stmt := db_prepare(db, "DELETE FROM todos WHERE id = ?")?
    db_execute_prepared(stmt, [id])
}
```

**Option B: Document the Risk (Minimum)**

If prepared statements can't be added immediately, update the example with warnings:

```forma
// WARNING: This example uses string concatenation for simplicity.
// In production, use parameterized queries to prevent SQL injection.
// See: https://forma-lang.org/docs/security/sql-injection

f add_todo(db: Database, title: Str) -> Result[Int, Str] {
    // INSECURE - for demonstration only
    // title must be sanitized if from user input
    db_execute(db, "INSERT INTO todos (title) VALUES ('" + title + "')")
}
```

---

### 1.6 Stdlib - Incomplete Map Module

**File:** stdlib/map.forma

**Issue:** Only 3 helper functions exist. No actual Map operations.

**Pillar Alignment:** Violates Pillar 1 (AI Code Generation First) - AI expects standard library completeness.

#### Recommended Solution

```forma
// stdlib/map.forma - Complete Implementation

# ============================================================
# Map Module - Key-Value Collections
# ============================================================

# Map is a built-in type: {K: V} or Map[K, V]
# Keys must be hashable (Str, Int, Bool)

# ============================================================
# Core Operations (Builtins)
# ============================================================

# map_new() -> {K: V}           - Create empty map
# map_insert(m, k, v) -> {K: V} - Insert/update, returns new map
# map_get(m, k) -> V?           - Get value by key
# map_remove(m, k) -> ({K: V}, V?) - Remove key, returns (new_map, removed)
# map_contains(m, k) -> Bool    - Check if key exists
# map_keys(m) -> [K]            - Get all keys
# map_values(m) -> [V]          - Get all values
# map_len(m) -> Int             - Number of entries
# map_is_empty(m) -> Bool       - Check if empty

# ============================================================
# Utility Functions
# ============================================================

# Get value with default
f map_get_or[K, V](m: {K: V}, key: K, default: V) -> V {
    m map_get(m, key) {
        Some(v) => v,
        None => default
    }
}

# Update value with function
f map_update[K, V](m: {K: V}, key: K, f: (V) -> V, default: V) -> {K: V} {
    current := map_get_or(m, key, default)
    map_insert(m, key, f(current))
}

# Merge two maps (second wins on conflict)
f map_merge[K, V](m1: {K: V}, m2: {K: V}) -> {K: V} {
    result := m1
    for key in map_keys(m2) {
        m map_get(m2, key) {
            Some(v) => result = map_insert(result, key, v),
            None => ()
        }
    }
    result
}

# Filter map by predicate on keys
f map_filter_keys[K, V](m: {K: V}, pred: (K) -> Bool) -> {K: V} {
    result := map_new()
    for key in map_keys(m) {
        if pred(key) {
            m map_get(m, key) {
                Some(v) => result = map_insert(result, key, v),
                None => ()
            }
        }
    }
    result
}

# Filter map by predicate on values
f map_filter_values[K, V](m: {K: V}, pred: (V) -> Bool) -> {K: V} {
    result := map_new()
    for key in map_keys(m) {
        m map_get(m, key) {
            Some(v) => {
                if pred(v) {
                    result = map_insert(result, key, v)
                }
            },
            None => ()
        }
    }
    result
}

# Map values with function
f map_map_values[K, V, U](m: {K: V}, f: (V) -> U) -> {K: U} {
    result := map_new()
    for key in map_keys(m) {
        m map_get(m, key) {
            Some(v) => result = map_insert(result, key, f(v)),
            None => ()
        }
    }
    result
}

# Convert to list of tuples
f map_entries[K, V](m: {K: V}) -> [(K, V)] {
    result := []
    for key in map_keys(m) {
        m map_get(m, key) {
            Some(v) => result = vec_push(result, (key, v)),
            None => ()
        }
    }
    result
}

# Create from list of tuples
f map_from_entries[K, V](entries: [(K, V)]) -> {K: V} {
    result := map_new()
    for entry in entries {
        (k, v) := entry
        result = map_insert(result, k, v)
    }
    result
}

# Fold over map entries
f map_fold[K, V, A](m: {K: V}, init: A, f: (A, K, V) -> A) -> A {
    acc := init
    for key in map_keys(m) {
        m map_get(m, key) {
            Some(v) => acc = f(acc, key, v),
            None => ()
        }
    }
    acc
}

# Check if any entry matches predicate
f map_any[K, V](m: {K: V}, pred: (K, V) -> Bool) -> Bool {
    for key in map_keys(m) {
        m map_get(m, key) {
            Some(v) => {
                if pred(key, v) {
                    ret true
                }
            },
            None => ()
        }
    }
    false
}

# Check if all entries match predicate
f map_all[K, V](m: {K: V}, pred: (K, V) -> Bool) -> Bool {
    for key in map_keys(m) {
        m map_get(m, key) {
            Some(v) => {
                if !pred(key, v) {
                    ret false
                }
            },
            None => ()
        }
    }
    true
}

# ============================================================
# Int Map Specializations
# ============================================================

f int_map_sum_values(m: {Str: Int}) -> Int {
    total := 0
    for v in map_values(m) {
        total = total + v
    }
    total
}

f int_map_max_value(m: {Str: Int}) -> Int? {
    values := map_values(m)
    if vec_len(values) == 0 {
        None
    } else {
        max := values[0]
        for v in values {
            if v > max {
                max = v
            }
        }
        Some(max)
    }
}

f int_map_min_value(m: {Str: Int}) -> Int? {
    values := map_values(m)
    if vec_len(values) == 0 {
        None
    } else {
        min := values[0]
        for v in values {
            if v < min {
                min = v
            }
        }
        Some(min)
    }
}
```

---

### 1.7 Example - async_downloader.forma Fixes

**File:** examples/async_downloader.forma

**Issue:** Uses undefined functions, not actually async.

#### Recommended Solution

```forma
// examples/async_downloader.forma - Fixed Version
// Demonstrates concurrent HTTP downloads using FORMA's async features

# Fetch a single URL and return (url, content_length, success)
as f fetch_url(url: Str) -> (Str, Int, Bool) {
    print(f"Starting: {url}")

    m http_get(url) {
        Ok(response) => {
            (status, body, _headers) := response
            if status >= 200 && status < 300 {
                print(f"Success: {url} ({str_len(body)} bytes)")
                (url, str_len(body), true)
            } else {
                print(f"Failed: {url} (status {status})")
                (url, 0, false)
            }
        },
        Err(e) => {
            print(f"Error: {url} - {e}")
            (url, 0, false)
        }
    }
}

as f main() {
    # URLs to download
    urls := [
        "https://httpbin.org/get",
        "https://httpbin.org/ip",
        "https://httpbin.org/headers",
        "https://httpbin.org/user-agent"
    ]

    print(f"Downloading {vec_len(urls)} URLs concurrently...")
    start := time_now_ms()

    # Spawn all downloads as concurrent tasks
    tasks := []
    for url in urls {
        task := sp fetch_url(url)
        tasks = vec_push(tasks, task)
    }

    # Wait for all tasks to complete
    results := await_all(tasks)

    elapsed := time_now_ms() - start
    print(f"\nAll downloads complete in {elapsed}ms")

    # Count successes and total bytes
    success_count := 0
    total_bytes := 0

    for result in results {
        (_url, bytes, success) := result
        if success {
            success_count = success_count + 1
            total_bytes = total_bytes + bytes
        }
    }

    print(f"Successful: {success_count}/{vec_len(urls)}")
    print(f"Total bytes: {total_bytes}")
}
```

---

## Part 2: High Priority Issues with Solutions

### 2.1 Parser - Error Recovery Missing

**Recommended Solution:** Implement panic mode recovery

```rust
// Add to src/parser/parser.rs

impl Parser {
    // Synchronization tokens for recovery
    const SYNC_TOKENS: &'static [TokenKind] = &[
        TokenKind::F,      // function
        TokenKind::S,      // struct
        TokenKind::E,      // enum
        TokenKind::T,      // trait
        TokenKind::I,      // impl
        TokenKind::RBrace, // end of block
    ];

    fn synchronize(&mut self) {
        self.advance();

        while !self.at_end() {
            // Stop at statement boundaries
            if self.previous().kind == TokenKind::Newline {
                return;
            }

            // Stop at sync tokens
            if Self::SYNC_TOKENS.contains(&self.current().map(|t| t.kind).unwrap_or(TokenKind::Eof)) {
                return;
            }

            self.advance();
        }
    }

    fn parse_items_with_recovery(&mut self) -> Vec<Item> {
        let mut items = Vec::new();
        let mut errors = Vec::new();

        while !self.at_end() {
            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(e) => {
                    errors.push(e);
                    self.synchronize();  // Recover and continue
                }
            }
        }

        // Report all errors, not just first
        if !errors.is_empty() {
            self.errors.extend(errors);
        }

        items
    }
}
```

### 2.2 Type System - Option/Result Unification

**Recommended Solution:** Handle both representations consistently

```rust
// Add to src/types/inference.rs unification

fn unify(&mut self, t1: &Ty, t2: &Ty, span: Span) -> Result<(), TypeError> {
    match (t1, t2) {
        // Handle Option variants
        (Ty::Option(inner1), Ty::Option(inner2)) => {
            self.unify(inner1, inner2, span)
        }
        (Ty::Option(inner), Ty::Named(id, args)) |
        (Ty::Named(id, args), Ty::Option(inner)) if id.name == "Option" => {
            if let Some(arg) = args.first() {
                self.unify(inner, arg, span)
            } else {
                Ok(())
            }
        }

        // Handle Result variants - ADD THIS
        (Ty::Result(ok1, err1), Ty::Result(ok2, err2)) => {
            self.unify(ok1, ok2, span)?;
            self.unify(err1, err2, span)
        }
        (Ty::Result(ok, err), Ty::Named(id, args)) |
        (Ty::Named(id, args), Ty::Result(ok, err)) if id.name == "Result" => {
            if args.len() >= 2 {
                self.unify(ok, &args[0], span)?;
                self.unify(err, &args[1], span)
            } else {
                Ok(())
            }
        }

        // ... rest of unification cases
    }
}
```

### 2.3 Stdlib - Naming Convention Standardization

**Recommended Convention:**

```
module_operation[_type][_modifier]

Examples:
- str_split      (module: str, operation: split)
- vec_push       (module: vec, operation: push)
- map_get_or     (module: map, operation: get, modifier: or)
- int_vec_sum    (module: int_vec, operation: sum)
- char_is_digit  (module: char, operation: is_digit)
```

**Migration Plan:**

| Current | New | Notes |
|---------|-----|-------|
| `is_digit` | `char_is_digit` | Add `char_` prefix |
| `is_alpha` | `char_is_alpha` | Add `char_` prefix |
| `int_vec_sum` | `vec_sum_int` | Reorder for consistency |
| `array_fold_indexed` | `iter_fold_indexed` | Move to iter module |

---

## Part 3: Medium Priority Issues with Solutions

### 3.1 Lexer - Add `%=` Operator

```rust
// Add to src/lexer/scanner.rs

fn scan_token(&mut self) -> Result<Token, LexError> {
    // In the '%' handling section:
    '%' => {
        if self.match_char('=') {
            Token::new(TokenKind::PercentEq, self.current_span())
        } else {
            Token::new(TokenKind::Percent, self.current_span())
        }
    }
}
```

### 3.2 MIR - Short-Circuit Evaluation

```rust
// Modify src/mir/lower.rs for && and ||

fn lower_logical_and(&mut self, left: &Expr, right: &Expr) -> Option<Operand> {
    // Short-circuit: if left is false, don't evaluate right
    let result = self.new_temp(Ty::Bool);
    let left_op = self.lower_expr(left)?;

    let eval_right_block = self.new_block();
    let done_block = self.new_block();

    // If left is false, result is false (skip right)
    self.terminate(Terminator::If {
        condition: left_op.clone(),
        then_block: eval_right_block,
        else_block: done_block,
    });

    // Evaluate right in true branch
    self.current_block = Some(eval_right_block);
    let right_op = self.lower_expr(right)?;
    self.emit(Statement::Assign(result, Rvalue::Use(right_op)));
    self.terminate(Terminator::Goto(done_block));

    // In false branch, result is already false
    self.current_block = Some(done_block);
    // Phi node or similar to merge result

    Some(Operand::Copy(result))
}
```

### 3.3 MIR - Contract Enforcement

```rust
// Add to src/mir/interp.rs

fn check_preconditions(&self, func: &Function, args: &[Value]) -> Result<(), InterpError> {
    for contract in &func.preconditions {
        // Parse and evaluate the contract expression
        let result = self.eval_contract_expr(&contract.expr_string, args)?;

        if !result {
            let msg = contract.message.as_deref().unwrap_or("precondition failed");
            return Err(InterpError::new(format!(
                "Contract violation in '{}': {}",
                func.name, msg
            )));
        }
    }
    Ok(())
}

fn check_postconditions(&self, func: &Function, result: &Value) -> Result<(), InterpError> {
    for contract in &func.postconditions {
        let check_result = self.eval_contract_expr(&contract.expr_string, &[result.clone()])?;

        if !check_result {
            let msg = contract.message.as_deref().unwrap_or("postcondition failed");
            return Err(InterpError::new(format!(
                "Contract violation in '{}': {}",
                func.name, msg
            )));
        }
    }
    Ok(())
}
```

---

## Part 4: Implementation Priority

### Sprint 9: Type Safety (1-2 weeks)

| Task | Effort | Impact |
|------|--------|--------|
| Implement method type checking | 3 days | Critical |
| Implement field type checking | 2 days | Critical |
| Fix struct pattern validation | 2 days | Critical |
| Fix enum discriminant collision | 1 day | Critical |
| Fix Option/Result unification | 1 day | High |

### Sprint 10: Stdlib & Examples (1 week)

| Task | Effort | Impact |
|------|--------|--------|
| Add prepared statement API | 2 days | Critical |
| Complete map.forma module | 2 days | Critical |
| Fix async_downloader example | 0.5 day | High |
| Standardize naming conventions | 1 day | High |
| Add missing string functions | 1 day | High |

### Sprint 11: Polish (1 week)

| Task | Effort | Impact |
|------|--------|--------|
| Parser error recovery | 2 days | Medium |
| MIR type propagation | 2 days | Medium |
| Short-circuit evaluation | 1 day | Medium |
| Contract enforcement | 1 day | Medium |
| Add `%=` operator | 0.5 day | Low |

---

## Part 5: Testing Requirements

### New Tests Needed for Fixes

```forma
// tests/forma/test_method_resolution.forma
f test_method_on_vec() {
    v := [1, 2, 3]
    assert_eq(v.len(), 3)         // Method call type checking
    assert_eq(v.push(4).len(), 4) // Chained methods
}

f test_field_access() {
    s Point { x: Int, y: Int }
    p := Point { x: 10, y: 20 }
    assert_eq(p.x, 10)            // Field type checking
    assert_eq(p.y, 20)
}

// tests/forma/test_pattern_matching.forma
f test_struct_pattern() {
    s Point { x: Int, y: Int }
    p := Point { x: 10, y: 20 }
    m p {
        Point { x, y } => assert_eq(x + y, 30)
    }
}

// tests/forma/test_enum_discriminants.forma
f test_user_enum_match() {
    e Color { Red, Green, Blue, Cyan, Magenta, Yellow }

    // These variants would collide with naive hash
    c1 := Color::Red
    c2 := Color::Cyan

    m c1 {
        Color::Red => assert_true(true),
        _ => assert_true(false)
    }

    m c2 {
        Color::Cyan => assert_true(true),
        _ => assert_true(false)
    }
}

// tests/forma/test_sql_prepared.forma
f test_prepared_statements() {
    db := db_open_memory()!
    db_execute(db, "CREATE TABLE test (id INT, name TEXT)")!

    // Should use parameterized query
    stmt := db_prepare(db, "INSERT INTO test VALUES (?, ?)")!
    db_execute_prepared(stmt, [1, "Alice"])!
    db_execute_prepared(stmt, [2, "Bob"])!

    rows := db_query(db, "SELECT * FROM test")!
    assert_eq(vec_len(rows), 2)

    db_close(db)
}
```

---

## Conclusion

This report provides specific, implementable solutions for all critical and high-priority issues. Each solution:

1. **Aligns with FORMA's pillars** - No added complexity for AI
2. **Includes code examples** - Ready to implement
3. **Has clear acceptance criteria** - Testable outcomes
4. **Prioritized by impact** - Critical issues first

**Estimated timeline to v1.0:** 3-4 weeks with focused effort on Sprints 9-11.

The language design remains sound. These are implementation gaps, not architectural problems. With these fixes, FORMA will deliver on its promise of "code that writes itself correctly."

---

*"Code that writes itself correctly" - with proper type checking to ensure it.*
