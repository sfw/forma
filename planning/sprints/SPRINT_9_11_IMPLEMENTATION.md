# FORMA Implementation Sprint 9-11: Code Review Fixes

**For:** Claude Code Implementation
**Date:** January 25, 2026
**Source:** CODE_REVIEW_REPORT.md

---

## Overview

This document provides implementation tasks for fixing critical issues identified in the FORMA code review. All solutions must preserve FORMA's core pillars:

1. **AI Code Generation First** - No lifetimes, predictable patterns, minimal annotations
2. **Memory Safety Without Lifetimes** - Second-class references, no stored references
3. **Token Efficiency** - Short keywords, concise syntax
4. **Machine-Readable Tooling** - Grammar export, structured JSON errors
5. **Strong Type Inference** - Hindley-Milner style, minimal annotations needed

---

## Sprint 9: Type Safety (Critical)

### Task 9.1: Implement Method Type Checking

**File:** `src/types/inference.rs` (around lines 3272-3286)

**Problem:** Method calls return fresh type variables instead of actual types. No type checking occurs.

**Current broken code:**
```rust
ExprKind::MethodCall(receiver, _method, args) => {
    let _receiver_ty = self.infer_expr(receiver)?;
    Ok(Ty::fresh_var())  // Returns unknown type - NO TYPE CHECKING!
}
```

**Solution:** Implement proper method resolution with receiver type lookup.

```rust
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
```

**Add helper method:**
```rust
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

**You may need to add:**
- A `MethodSignature` struct if it doesn't exist
- A `builtin_methods` HashMap to the inference context
- Registration of builtin methods for Vec, Map, String, etc.

**Acceptance Criteria:**
- [ ] `v.push(1)` type checks the argument
- [ ] `v.nonexistent()` produces error with available methods listed
- [ ] Chained methods like `v.push(1).len()` work correctly

---

### Task 9.2: Implement Field Type Checking

**File:** `src/types/inference.rs` (around lines 3283-3286)

**Problem:** Field access returns fresh type variable instead of actual field type.

**Solution:**
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

**Acceptance Criteria:**
- [ ] `point.x` returns the correct type for field `x`
- [ ] `point.nonexistent` produces helpful error
- [ ] Tuple field access like `tuple.0` works

---

### Task 9.3: Fix Struct Pattern Validation

**File:** `src/types/inference.rs` (around lines 3779-3785)

**Problem:** Struct patterns don't validate against actual struct field types.

**Solution:**
```rust
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

**Acceptance Criteria:**
- [ ] `Point { x, y }` validates x and y exist
- [ ] `Point { x, z }` produces error for unknown field z
- [ ] `Point { x, .. }` allows partial matching

---

### Task 9.4: Fix Enum Discriminant Hash Collision

**File:** `src/mir/interp.rs` (around lines 5508-5512)

**Problem:** Uses naive byte sum for discriminants, causing collisions (e.g., "ab" and "ba" hash the same).

**Solution:** Use index-based discriminants per enum.

```rust
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
                    panic!("unregistered enum variant: {}::{}", enum_name, variant_name)
                })
            }
        }
    }
}
```

**Update Interpreter struct:**
```rust
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

**Acceptance Criteria:**
- [ ] Enum variants with same character sum match correctly
- [ ] User enums with 10+ variants work
- [ ] Built-in Option/Result still work

---

### Task 9.5: Fix Option/Result Unification

**File:** `src/types/inference.rs` (in unification code)

**Problem:** `Ty::Option(T)` and `Ty::Named("Option", [T])` don't unify properly.

**Solution:** Add special cases for both representations.

```rust
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

        // Handle Result variants
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

**Acceptance Criteria:**
- [ ] `Option[Int]` unifies with `Int?`
- [ ] `Result[T, E]` unifies with `T!`
- [ ] Generic functions returning Option/Result work

---

## Sprint 10: Stdlib & Examples

### Task 10.1: Add Prepared Statement API

**File:** `src/mir/interp.rs` (in builtins section)

**Add new builtins:**
```rust
"db_prepare" => {
    let db_id = args[0].as_db()?;
    let sql = args[1].as_str()?;

    let db = self.databases.get(&db_id)
        .ok_or_else(|| InterpError::new("invalid database handle"))?;

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

"db_query_prepared" => {
    let stmt_id = args[0].as_stmt()?;
    let params = args[1].as_vec()?;

    // Similar to db_execute_prepared but returns rows
    // ... implementation
}
```

**You need to add:**
- `statements: HashMap<u64, Statement>` to Interpreter
- `next_stmt_id: u64` counter
- `Value::Statement(u64)` variant

**Acceptance Criteria:**
- [ ] `db_prepare` creates reusable statement
- [ ] `db_execute_prepared` safely binds parameters
- [ ] No SQL injection possible with parameters

---

### Task 10.2: Fix examples/cli_with_db.forma

**File:** `examples/cli_with_db.forma`

**Update to use prepared statements:**
```forma
f add_todo(db: Database, title: Str) -> Result[Int, Str] {
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

---

### Task 10.3: Complete stdlib/map.forma

**File:** `stdlib/map.forma`

Replace with complete implementation from CODE_REVIEW_REPORT.md Part 1.6. Key functions to add:
- `map_get_or[K, V](m, key, default) -> V`
- `map_update[K, V](m, key, f, default) -> {K: V}`
- `map_merge[K, V](m1, m2) -> {K: V}`
- `map_filter_keys[K, V](m, pred) -> {K: V}`
- `map_filter_values[K, V](m, pred) -> {K: V}`
- `map_map_values[K, V, U](m, f) -> {K: U}`
- `map_entries[K, V](m) -> [(K, V)]`
- `map_from_entries[K, V](entries) -> {K: V}`
- `map_fold[K, V, A](m, init, f) -> A`
- `map_any[K, V](m, pred) -> Bool`
- `map_all[K, V](m, pred) -> Bool`
- `int_map_sum_values(m) -> Int`
- `int_map_max_value(m) -> Int?`
- `int_map_min_value(m) -> Int?`

---

### Task 10.4: Fix examples/async_downloader.forma

**File:** `examples/async_downloader.forma`

Replace with fixed version from CODE_REVIEW_REPORT.md Part 1.7. Key fixes:
- Use `as f` for async functions
- Use `sp` for spawn
- Use actual http_get response structure `(status, body, headers)`
- Properly await tasks with `await_all`

---

## Sprint 11: Polish

### Task 11.1: Parser Error Recovery

**File:** `src/parser/parser.rs`

**Add synchronization for error recovery:**
```rust
impl Parser {
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
            if self.previous().kind == TokenKind::Newline {
                return;
            }

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
                    self.synchronize();
                }
            }
        }

        if !errors.is_empty() {
            self.errors.extend(errors);
        }

        items
    }
}
```

---

### Task 11.2: MIR Type Propagation

**File:** `src/mir/lower.rs`

**Add type inference for temporaries:**
```rust
fn infer_rvalue_type(&self, rvalue: &Rvalue) -> Ty {
    match rvalue {
        Rvalue::Use(operand) => self.operand_type(operand),
        Rvalue::BinaryOp(op, left, right) => {
            let left_ty = self.operand_type(left);
            match op {
                BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => Ty::Bool,
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
            self.function_return_type(func).unwrap_or(Ty::Unit)
        }
    }
}
```

---

### Task 11.3: Short-Circuit Evaluation for && and ||

**File:** `src/mir/lower.rs`

**Implement proper short-circuit:**
```rust
fn lower_logical_and(&mut self, left: &Expr, right: &Expr) -> Option<Operand> {
    let result = self.new_temp(Ty::Bool);
    let left_op = self.lower_expr(left)?;

    let eval_right_block = self.new_block();
    let done_block = self.new_block();

    self.terminate(Terminator::If {
        condition: left_op.clone(),
        then_block: eval_right_block,
        else_block: done_block,
    });

    self.current_block = Some(eval_right_block);
    let right_op = self.lower_expr(right)?;
    self.emit(Statement::Assign(result, Rvalue::Use(right_op)));
    self.terminate(Terminator::Goto(done_block));

    self.current_block = Some(done_block);
    Some(Operand::Copy(result))
}
```

---

### Task 11.4: Add `%=` Operator

**File:** `src/lexer/scanner.rs`

```rust
'%' => {
    if self.match_char('=') {
        Token::new(TokenKind::PercentEq, self.current_span())
    } else {
        Token::new(TokenKind::Percent, self.current_span())
    }
}
```

Also update parser to handle compound assignment.

---

## Testing Requirements

After each task, run:
```bash
cargo test
```

**Add new tests for each fix:**

```forma
// tests/forma/test_type_safety.forma

f test_method_resolution() {
    v := [1, 2, 3]
    assert_eq(vec_len(v), 3)
}

f test_field_access() {
    s Point { x: Int, y: Int }
    p := Point { x: 10, y: 20 }
    assert_eq(p.x, 10)
}

f test_struct_pattern() {
    s Point { x: Int, y: Int }
    p := Point { x: 10, y: 20 }
    m p {
        Point { x, y } => assert_eq(x + y, 30)
    }
}

f test_enum_discriminants() {
    e Color { Red, Green, Blue, Cyan, Magenta, Yellow }

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

f test_prepared_statements() {
    db := db_open_memory()!
    db_execute(db, "CREATE TABLE test (id INT, name TEXT)")!

    stmt := db_prepare(db, "INSERT INTO test VALUES (?, ?)")!
    db_execute_prepared(stmt, [1, "Alice"])!

    rows := db_query(db, "SELECT * FROM test")!
    assert_eq(vec_len(rows), 1)

    db_close(db)
}
```

---

## Deferred to v1.1

### Task 11.2: MIR Type Propagation

**Status:** Deferred to v1.1

**Rationale:**
- Medium priority - not critical for v1.0 functionality
- Risk of regressions in working interpreter
- Only needed for future LLVM codegen optimization
- Current `Ty::Int` placeholder works correctly at runtime
- Type information is available in HIR for any tooling needs

**Future Work:**
- Implement `infer_rvalue_type()` for proper MIR type propagation
- Add `operand_type()` and `function_return_type()` helpers
- Update all `new_temp()` calls to use inferred types
- Required before LLVM backend implementation

---

## Definition of Done

- [x] All `cargo test` passes (345+ tests)
- [x] Sprint 9 type safety tasks complete
- [x] Sprint 10 stdlib tasks complete
- [x] Sprint 11 polish tasks complete (11.2 deferred to v1.1)
- [x] Examples run without errors
- [x] No regression in existing functionality

---

*FORMA: Code that writes itself correctly - with proper type checking to ensure it.*
