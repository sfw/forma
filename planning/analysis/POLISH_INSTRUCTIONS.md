# FORMA Compiler Polish Instructions

## Objective
Take the FORMA compiler from B+ quality to A/A+ quality. Complete these tasks BEFORE starting on SELF_HOSTING_PLAN_EXPANDED.md.

## Current State Assessment

| Component | Current | Target | Priority |
|-----------|---------|--------|----------|
| Type lowering | Broken (returns Int for everything) | Fully working | CRITICAL |
| Enum in MIR | Missing | Complete | CRITICAL |
| Pattern matching | Int literals only | Full enums + destructuring | CRITICAL |
| Type checker integration | Not called in `run` | Integrated | HIGH |
| Borrow checker integration | Not called in `run` | Integrated | HIGH |
| Test coverage | ~60% | 90%+ | HIGH |
| Error messages | Basic | Production quality | MEDIUM |
| Documentation | Sparse | Comprehensive | MEDIUM |
| CLI completeness | Partial | Full | LOW |

---

## Phase 1: Critical Fixes (Do These First)

### 1.1 Fix `lower_type()` in src/mir/lower.rs

**Location:** Around line 924-927

**Current broken code:**
```rust
fn lower_type(&self, _ty: &crate::parser::Type) -> Ty {
    // TODO: proper type lowering
    Ty::Int
}
```

**Replace with:**
```rust
fn lower_type(&self, ty: &crate::parser::Type) -> Ty {
    use crate::parser::TypeKind as AstTypeKind;

    match &ty.kind {
        AstTypeKind::Path(path) => {
            // Get the first segment (e.g., "Int", "Option", "MyStruct")
            let first_seg = &path.segments[0];
            let name = &first_seg.name.name;

            // Lower any generic arguments
            let type_args: Vec<Ty> = first_seg.args.as_ref()
                .map(|args| {
                    args.args.iter().filter_map(|arg| {
                        match arg {
                            crate::parser::GenericArg::Type(t) => Some(self.lower_type(t)),
                            _ => None,
                        }
                    }).collect()
                })
                .unwrap_or_default();

            // Match built-in types
            match name.as_str() {
                "Int" => Ty::Int,
                "Bool" => Ty::Bool,
                "Str" => Ty::Str,
                "Float" => Ty::Float,
                "Char" => Ty::Char,
                "Unit" | "()" => Ty::Unit,
                // Generic built-ins
                "Option" => {
                    if type_args.len() == 1 {
                        Ty::Option(Box::new(type_args[0].clone()))
                    } else {
                        Ty::Option(Box::new(Ty::Error))
                    }
                }
                "Result" => {
                    let ok_ty = type_args.get(0).cloned().unwrap_or(Ty::Error);
                    let err_ty = type_args.get(1).cloned().unwrap_or(Ty::Str);
                    Ty::Result(Box::new(ok_ty), Box::new(err_ty))
                }
                // User-defined type
                _ => Ty::Named(name.clone(), type_args),
            }
        }

        AstTypeKind::List(inner) => {
            Ty::List(Box::new(self.lower_type(inner)))
        }

        AstTypeKind::Option(inner) => {
            Ty::Option(Box::new(self.lower_type(inner)))
        }

        AstTypeKind::Result(ok, err) => {
            let ok_ty = self.lower_type(ok);
            let err_ty = err.as_ref()
                .map(|e| self.lower_type(e))
                .unwrap_or(Ty::Str);
            Ty::Result(Box::new(ok_ty), Box::new(err_ty))
        }

        AstTypeKind::Tuple(tys) => {
            Ty::Tuple(tys.iter().map(|t| self.lower_type(t)).collect())
        }

        AstTypeKind::Ref(inner, is_mut) => {
            let mutability = if *is_mut {
                super::mir::Mutability::Mutable
            } else {
                super::mir::Mutability::Immutable
            };
            Ty::Ref(Box::new(self.lower_type(inner)), mutability)
        }

        AstTypeKind::Fn(params, ret) => {
            let param_tys: Vec<Ty> = params.iter().map(|t| self.lower_type(t)).collect();
            let ret_ty = self.lower_type(ret);
            Ty::Fn(param_tys, Box::new(ret_ty))
        }

        AstTypeKind::Infer => Ty::Var(crate::types::TypeVar::fresh()),

        AstTypeKind::Never => Ty::Never,

        _ => Ty::Error,
    }
}
```

**Note:** You may need to add `Ty::Named`, `Ty::Error`, `Ty::Never` variants to `src/types/types.rs` if they don't exist.

**Test this fix:**
```bash
# Create test file
cat > /tmp/test_types.forma << 'EOF'
f identity(x: Int) -> Int = x
f main() -> Int = identity(42)
EOF

cargo run -- run /tmp/test_types.forma
# Should output: 42
```

### 1.2 Add Enum Support to MIR

**In src/mir/mir.rs**, add to the `Rvalue` enum:
```rust
/// Enum variant construction: `Color::Red` or `Some(42)`
Enum {
    type_name: String,
    variant: String,
    fields: Vec<Operand>,
},
```

**In src/mir/mir.rs**, add Display implementation:
```rust
// In impl fmt::Display for Rvalue
Rvalue::Enum { type_name, variant, fields } => {
    write!(f, "{}::{}", type_name, variant)?;
    if !fields.is_empty() {
        write!(f, "(")?;
        for (i, field) in fields.iter().enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{}", field)?;
        }
        write!(f, ")")?;
    }
    Ok(())
}
```

**In src/mir/interp.rs**, add to the `Value` enum:
```rust
/// Enum variant value
Enum {
    type_name: String,
    variant: String,
    fields: Vec<Value>,
},
```

**In src/mir/interp.rs**, add Display implementation:
```rust
// In impl fmt::Display for Value
Value::Enum { type_name, variant, fields } => {
    write!(f, "{}::{}", type_name, variant)?;
    if !fields.is_empty() {
        write!(f, "(")?;
        for (i, v) in fields.iter().enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{}", v)?;
        }
        write!(f, ")")?;
    }
    Ok(())
}
```

**In src/mir/interp.rs**, handle enum construction in `eval_rvalue`:
```rust
Rvalue::Enum { type_name, variant, fields } => {
    let field_vals: Vec<Value> = fields
        .iter()
        .map(|f| self.eval_operand(f))
        .collect::<Result<_, _>>()?;
    Ok(Value::Enum {
        type_name: type_name.clone(),
        variant: variant.clone(),
        fields: field_vals,
    })
}
```

### 1.3 Lower Enum Expressions

**In src/mir/lower.rs**, find `lower_expr` and handle enum variant construction:

When you see a path expression like `Color::Red` or `Some(42)`:
```rust
ExprKind::Path(path) => {
    // Check if this is an enum variant
    if path.segments.len() == 2 {
        // Looks like EnumType::Variant
        let type_name = &path.segments[0].name.name;
        let variant = &path.segments[1].name.name;

        // Unit variant (no fields)
        let result = self.add_local(Ty::Named(type_name.clone(), vec![]));
        self.push_stmt(StatementKind::Assign(
            result,
            Rvalue::Enum {
                type_name: type_name.clone(),
                variant: variant.clone(),
                fields: vec![],
            },
        ));
        return Ok(result);
    }
    // ... existing path handling
}

ExprKind::Call { func, args } => {
    // Check if func is an enum variant constructor like Some(x)
    if let ExprKind::Path(path) = &func.kind {
        // Could be Option::Some(x) or just Some(x)
        let (type_name, variant) = if path.segments.len() == 2 {
            (path.segments[0].name.name.clone(), path.segments[1].name.name.clone())
        } else if path.segments.len() == 1 {
            // Infer type from context - for now assume Option for Some/None
            let variant = &path.segments[0].name.name;
            match variant.as_str() {
                "Some" | "None" => ("Option".to_string(), variant.clone()),
                "Ok" | "Err" => ("Result".to_string(), variant.clone()),
                _ => {
                    // Not an enum constructor, fall through to regular call
                    return self.lower_regular_call(func, args);
                }
            }
        } else {
            return self.lower_regular_call(func, args);
        };

        // Lower arguments
        let field_operands: Vec<Operand> = args
            .iter()
            .map(|arg| self.lower_expr(arg).map(|l| Operand::Local(l)))
            .collect::<Result<_, _>>()?;

        let result = self.add_local(Ty::Named(type_name.clone(), vec![]));
        self.push_stmt(StatementKind::Assign(
            result,
            Rvalue::Enum {
                type_name,
                variant,
                fields: field_operands,
            },
        ));
        return Ok(result);
    }
    // ... existing call handling
}
```

### 1.4 Enum Pattern Matching

**In src/mir/lower.rs**, update `lower_match` to handle enum patterns:

```rust
fn lower_match(&mut self, scrutinee: &Expr, arms: &[MatchArm]) -> Result<Local, LowerError> {
    let scrut_local = self.lower_expr(scrutinee)?;
    let result_local = self.add_local(Ty::Unit); // Will be unified later

    let exit_block = self.add_block();

    // For each arm, create a test block and body block
    let mut arm_blocks: Vec<(BlockId, BlockId)> = vec![];

    for arm in arms {
        let test_block = self.add_block();
        let body_block = self.add_block();
        arm_blocks.push((test_block, body_block));
    }

    // Chain: test1 -> (match: body1, else: test2) -> (match: body2, else: test3) -> ...
    for (i, (arm, (test_block, body_block))) in arms.iter().zip(arm_blocks.iter()).enumerate() {
        self.set_current_block(*test_block);

        let next_test = if i + 1 < arm_blocks.len() {
            arm_blocks[i + 1].0
        } else {
            // Last arm - should be wildcard or exhaustive
            *body_block // Fall through to body if no match
        };

        match &arm.pattern.kind {
            PatternKind::Wildcard => {
                // Always matches, go directly to body
                self.terminate(Terminator::Goto(*body_block));
            }

            PatternKind::Identifier(name) => {
                // Bind the value and go to body
                let local = self.add_local(Ty::Unit);
                self.vars.insert(name.name.clone(), local);
                self.push_stmt(StatementKind::Assign(
                    local,
                    Rvalue::Use(Operand::Copy(scrut_local)),
                ));
                self.terminate(Terminator::Goto(*body_block));
            }

            PatternKind::Literal(lit) => {
                // Compare and branch
                let lit_local = self.lower_literal(lit)?;
                let cond_local = self.add_local(Ty::Bool);
                self.push_stmt(StatementKind::Assign(
                    cond_local,
                    Rvalue::BinaryOp(BinOp::Eq, Operand::Copy(scrut_local), Operand::Copy(lit_local)),
                ));
                self.terminate(Terminator::If {
                    cond: Operand::Copy(cond_local),
                    then_block: *body_block,
                    else_block: next_test,
                });
            }

            PatternKind::Path(path) => {
                // Enum unit variant: Color::Red or None
                let variant = if path.segments.len() == 2 {
                    &path.segments[1].name.name
                } else {
                    &path.segments[0].name.name
                };

                // Check discriminant
                let disc_local = self.add_local(Ty::Int);
                self.push_stmt(StatementKind::Assign(
                    disc_local,
                    Rvalue::Discriminant(scrut_local),
                ));

                let variant_disc = self.get_variant_discriminant(variant);
                let expected = self.add_local(Ty::Int);
                self.push_stmt(StatementKind::Assign(
                    expected,
                    Rvalue::Use(Operand::Constant(Constant::Int(variant_disc))),
                ));

                let cond = self.add_local(Ty::Bool);
                self.push_stmt(StatementKind::Assign(
                    cond,
                    Rvalue::BinaryOp(BinOp::Eq, Operand::Copy(disc_local), Operand::Copy(expected)),
                ));

                self.terminate(Terminator::If {
                    cond: Operand::Copy(cond),
                    then_block: *body_block,
                    else_block: next_test,
                });
            }

            PatternKind::TupleStruct { path, fields } => {
                // Enum with fields: Some(x) or Ok(value)
                let variant = if path.segments.len() == 2 {
                    &path.segments[1].name.name
                } else {
                    &path.segments[0].name.name
                };

                // Check discriminant
                let disc_local = self.add_local(Ty::Int);
                self.push_stmt(StatementKind::Assign(
                    disc_local,
                    Rvalue::Discriminant(scrut_local),
                ));

                let variant_disc = self.get_variant_discriminant(variant);
                let expected = self.add_local(Ty::Int);
                self.push_stmt(StatementKind::Assign(
                    expected,
                    Rvalue::Use(Operand::Constant(Constant::Int(variant_disc))),
                ));

                let cond = self.add_local(Ty::Bool);
                self.push_stmt(StatementKind::Assign(
                    cond,
                    Rvalue::BinaryOp(BinOp::Eq, Operand::Copy(disc_local), Operand::Copy(expected)),
                ));

                // Branch to extraction block or next test
                let extract_block = self.add_block();
                self.terminate(Terminator::If {
                    cond: Operand::Copy(cond),
                    then_block: extract_block,
                    else_block: next_test,
                });

                // In extract block: bind fields and goto body
                self.set_current_block(extract_block);
                for (i, field_pat) in fields.iter().enumerate() {
                    if let PatternKind::Identifier(name) = &field_pat.kind {
                        let field_local = self.add_local(Ty::Unit);
                        self.vars.insert(name.name.clone(), field_local);
                        self.push_stmt(StatementKind::Assign(
                            field_local,
                            Rvalue::EnumField(scrut_local, i),
                        ));
                    }
                }
                self.terminate(Terminator::Goto(*body_block));
            }

            _ => {
                // Unsupported pattern, skip to next
                self.terminate(Terminator::Goto(next_test));
            }
        }

        // Lower body
        self.set_current_block(*body_block);
        let body_result = self.lower_expr(&arm.body)?;
        self.push_stmt(StatementKind::Assign(
            result_local,
            Rvalue::Use(Operand::Copy(body_result)),
        ));
        self.terminate(Terminator::Goto(exit_block));
    }

    // Start the chain
    let entry = self.current_block.unwrap();
    self.set_current_block(entry);
    self.terminate(Terminator::Goto(arm_blocks[0].0));

    self.set_current_block(exit_block);
    Ok(result_local)
}

// Helper: map variant names to discriminants
fn get_variant_discriminant(&self, variant: &str) -> i64 {
    // For built-in types
    match variant {
        "None" => 0,
        "Some" => 1,
        "Ok" => 0,
        "Err" => 1,
        // For user enums, look up in type registry
        _ => 0, // TODO: proper lookup
    }
}
```

**Also add to src/mir/mir.rs:**
```rust
// In Rvalue enum
/// Get discriminant of an enum value
Discriminant(Local),
/// Extract field from enum variant
EnumField(Local, usize),
```

**And handle in interpreter:**
```rust
Rvalue::Discriminant(local) => {
    let val = self.get_local(*local)?;
    match val {
        Value::Enum { variant, .. } => {
            let disc = match variant.as_str() {
                "None" => 0,
                "Some" => 1,
                "Ok" => 0,
                "Err" => 1,
                _ => 0, // TODO: proper lookup
            };
            Ok(Value::Int(disc))
        }
        _ => Err(InterpError { message: "discriminant of non-enum".into() }),
    }
}

Rvalue::EnumField(local, idx) => {
    let val = self.get_local(*local)?;
    match val {
        Value::Enum { fields, .. } => {
            fields.get(*idx).cloned().ok_or_else(|| InterpError {
                message: format!("enum field {} out of bounds", idx),
            })
        }
        _ => Err(InterpError { message: "field access on non-enum".into() }),
    }
}
```

---

## Phase 2: Integration Fixes

### 2.1 Integrate Type Checker into Run Command

**In src/main.rs**, the `run` function should call the type checker:

```rust
fn run(file: &PathBuf) -> Result<(), String> {
    let source = read_file(file)?;
    let filename = file.to_string_lossy();
    let ctx = ErrorContext::new(&filename, &source);

    // Lex
    let scanner = Scanner::new(&source);
    let (tokens, lex_errors) = scanner.scan_all();
    if !lex_errors.is_empty() {
        for error in &lex_errors {
            ctx.error(error.span, &error.message);
        }
        return Err(format!("{} lexer error(s)", lex_errors.len()));
    }

    // Parse
    let parser = FormaParser::new(&tokens);
    let ast = parser.parse().map_err(|e| {
        ctx.error_with_help(e.span(), &format!("{}", e), e.help().unwrap_or("check syntax"));
        "parse error".to_string()
    })?;

    // ADD: Type check
    let mut type_checker = TypeChecker::new();
    if let Err(errors) = type_checker.check(&ast) {
        for error in &errors {
            ctx.error(error.span, &format!("{}", error));
        }
        return Err(format!("{} type error(s)", errors.len()));
    }

    // ADD: Borrow check
    let mut borrow_checker = BorrowChecker::new();
    if let Err(errors) = borrow_checker.check(&ast) {
        for error in &errors {
            ctx.error(error.span, &format!("{}", error));
        }
        return Err(format!("{} borrow error(s)", errors.len()));
    }

    // Lower to MIR
    let program = Lowerer::new().lower(&ast).map_err(|errors| {
        for e in &errors {
            ctx.error(e.span, &e.message);
        }
        format!("{} lowering error(s)", errors.len())
    })?;

    // ... rest unchanged
}
```

### 2.2 Add Missing Ty Variants

**In src/types/types.rs**, ensure these variants exist:

```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    // Primitives
    Unit,
    Bool,
    Int,
    Float,
    Char,
    Str,

    // Compound
    Tuple(Vec<Ty>),
    List(Box<Ty>),
    Option(Box<Ty>),
    Result(Box<Ty>, Box<Ty>),
    Fn(Vec<Ty>, Box<Ty>),
    Ref(Box<Ty>, Mutability),

    // User-defined
    Named(String, Vec<Ty>),  // ADD THIS: name + type arguments

    // Inference
    Var(TypeVar),

    // Special
    Never,  // ADD THIS: for diverging expressions
    Error,  // ADD THIS: for error recovery
}
```

---

## Phase 3: Test Coverage

### 3.1 Add MIR Lowering Tests

Create `tests/mir_tests.rs`:

```rust
use forma::{Scanner, Parser, Lowerer};

fn lower(source: &str) -> Result<aria::Program, String> {
    let scanner = Scanner::new(source);
    let (tokens, _) = scanner.scan_all();
    let parser = Parser::new(&tokens);
    let ast = parser.parse().map_err(|e| format!("{:?}", e))?;
    Lowerer::new().lower(&ast).map_err(|e| format!("{:?}", e))
}

#[test]
fn test_lower_simple_function() {
    let prog = lower("f main() -> Int = 42").unwrap();
    assert!(prog.functions.contains_key("main"));
}

#[test]
fn test_lower_with_correct_types() {
    let prog = lower("f add(a: Int, b: Int) -> Int = a + b").unwrap();
    let f = prog.functions.get("add").unwrap();
    assert_eq!(f.params.len(), 2);
    // Params should have Int type, not Error
    for (_, ty) in &f.params {
        assert_eq!(*ty, aria::Ty::Int);
    }
}

#[test]
fn test_lower_option_type() {
    let prog = lower(r#"
f maybe_int(b: Bool) -> Int?
    if b then Some(42) else None

f main() -> Int = 0
"#).unwrap();
    assert!(prog.functions.contains_key("maybe_int"));
}

#[test]
fn test_lower_enum_construction() {
    let prog = lower(r#"
e Color = Red | Green | Blue
f main() -> Color = Red
"#).unwrap();
    // Should compile without error
    assert!(prog.functions.contains_key("main"));
}

#[test]
fn test_lower_match_enum() {
    let prog = lower(r#"
f unwrap_or(opt: Int?, default: Int) -> Int
    m opt
        Some(x) => x
        None => default

f main() -> Int = unwrap_or(Some(42), 0)
"#).unwrap();
    assert!(prog.functions.contains_key("unwrap_or"));
}
```

### 3.2 Add Integration Tests

Create `tests/integration_tests.rs`:

```rust
use forma::{Scanner, Parser, Lowerer, Interpreter, Value};

fn run(source: &str) -> Result<Value, String> {
    let scanner = Scanner::new(source);
    let (tokens, lex_errors) = scanner.scan_all();
    if !lex_errors.is_empty() {
        return Err(format!("lex errors: {:?}", lex_errors));
    }

    let parser = Parser::new(&tokens);
    let ast = parser.parse().map_err(|e| format!("parse error: {:?}", e))?;

    let program = Lowerer::new().lower(&ast).map_err(|e| format!("lower error: {:?}", e))?;

    let mut interp = Interpreter::new(program);
    interp.run("main", &[]).map_err(|e| format!("runtime error: {}", e))
}

#[test]
fn test_option_some() {
    let result = run(r#"
f get_some() -> Int?
    Some(42)

f main() -> Int
    m get_some()
        Some(x) => x
        None => 0
"#).unwrap();
    assert_eq!(result, Value::Int(42));
}

#[test]
fn test_option_none() {
    let result = run(r#"
f get_none() -> Int?
    None

f main() -> Int
    m get_none()
        Some(x) => x
        None => -1
"#).unwrap();
    assert_eq!(result, Value::Int(-1));
}

#[test]
fn test_result_ok() {
    let result = run(r#"
f safe_div(a: Int, b: Int) -> Int!
    if b == 0 then Err("division by zero") else Ok(a / b)

f main() -> Int
    m safe_div(10, 2)
        Ok(x) => x
        Err(_) => -1
"#).unwrap();
    assert_eq!(result, Value::Int(5));
}

#[test]
fn test_user_enum() {
    let result = run(r#"
e Direction = North | South | East | West

f to_degrees(d: Direction) -> Int
    m d
        North => 0
        East => 90
        South => 180
        West => 270

f main() -> Int = to_degrees(East)
"#).unwrap();
    assert_eq!(result, Value::Int(90));
}

#[test]
fn test_enum_with_data() {
    let result = run(r#"
e Shape
    Circle(Int)
    Rectangle(Int, Int)

f area(s: Shape) -> Int
    m s
        Circle(r) => r * r * 3
        Rectangle(w, h) => w * h

f main() -> Int = area(Rectangle(4, 5))
"#).unwrap();
    assert_eq!(result, Value::Int(20));
}

#[test]
fn test_nested_match() {
    let result = run(r#"
f deep_unwrap(opt: Int??) -> Int
    m opt
        Some(inner) => m inner
            Some(x) => x
            None => -1
        None => -2

f main() -> Int = deep_unwrap(Some(Some(42)))
"#).unwrap();
    assert_eq!(result, Value::Int(42));
}

#[test]
fn test_list_type() {
    let result = run(r#"
f first_or_zero(xs: [Int]) -> Int
    if len(xs) > 0 then xs[0] else 0

f main() -> Int = first_or_zero([1, 2, 3])
"#).unwrap();
    assert_eq!(result, Value::Int(1));
}
```

### 3.3 Add Snapshot Tests

Add `insta` to Cargo.toml:
```toml
[dev-dependencies]
insta = "1.34"
```

Create `tests/snapshot_tests.rs`:
```rust
use forma::{Scanner, Parser, Lowerer};

fn mir_snapshot(source: &str) -> String {
    let scanner = Scanner::new(source);
    let (tokens, _) = scanner.scan_all();
    let parser = Parser::new(&tokens);
    let ast = parser.parse().unwrap();
    let program = Lowerer::new().lower(&ast).unwrap();
    format!("{}", program)
}

#[test]
fn snapshot_simple_function() {
    insta::assert_snapshot!(mir_snapshot("f main() -> Int = 42"));
}

#[test]
fn snapshot_if_expression() {
    insta::assert_snapshot!(mir_snapshot("f main() -> Int = if true then 1 else 0"));
}

#[test]
fn snapshot_while_loop() {
    insta::assert_snapshot!(mir_snapshot(r#"
f main() -> Int
    x := 0
    wh x < 10
        x = x + 1
    x
"#));
}

#[test]
fn snapshot_match_option() {
    insta::assert_snapshot!(mir_snapshot(r#"
f main() -> Int
    opt = Some(42)
    m opt
        Some(x) => x
        None => 0
"#));
}
```

Run `cargo insta test` and review/accept snapshots.

---

## Phase 4: Documentation

### 4.1 Module-Level Documentation

Add comprehensive doc comments to each module. Example for `src/mir/mod.rs`:

```rust
//! # Mid-level Intermediate Representation (MIR)
//!
//! MIR is the core intermediate representation used for:
//! - Interpretation (for testing and REPL)
//! - Future native code generation
//! - Optimization passes
//!
//! ## Architecture
//!
//! ```text
//! Source → Lexer → Parser → AST → Type Check → Borrow Check → MIR → Interpret/Codegen
//!                                                              ↑
//!                                                         You are here
//! ```
//!
//! ## Key Types
//!
//! - [`Program`]: A complete MIR program with all functions
//! - [`Function`]: A single function with basic blocks
//! - [`BasicBlock`]: A sequence of statements ending with a terminator
//! - [`Rvalue`]: An expression that produces a value
//! - [`Terminator`]: Control flow (return, branch, call)
//!
//! ## Example
//!
//! FORMA source:
//! ```forma
//! f add(a: Int, b: Int) -> Int = a + b
//! ```
//!
//! Becomes MIR:
//! ```text
//! fn add:
//!   params: [_0: Int, _1: Int]
//!   return: Int
//!
//!   bb0:
//!     _2 = _0 Add _1
//!     return _2
//! ```

pub mod interp;
pub mod lower;
pub mod mir;

pub use interp::{Interpreter, InterpError, Value};
pub use lower::{Lowerer, LowerError};
pub use mir::{Program, Function, BasicBlock, Rvalue, Terminator};
```

### 4.2 Add Examples to Key Functions

Add `# Examples` sections to public functions. For instance in `Interpreter::run`:

```rust
/// Run the program starting from the given function.
///
/// # Arguments
///
/// * `fn_name` - The name of the entry function (usually "main")
/// * `args` - Arguments to pass to the function
///
/// # Returns
///
/// The return value of the function, or an error if execution fails.
///
/// # Examples
///
/// ```
/// use forma::{Scanner, Parser, Lowerer, Interpreter};
///
/// let source = "f main() -> Int = 42";
/// let scanner = Scanner::new(source);
/// let (tokens, _) = scanner.scan_all();
/// let ast = Parser::new(&tokens).parse().unwrap();
/// let program = Lowerer::new().lower(&ast).unwrap();
///
/// let mut interp = Interpreter::new(program);
/// let result = interp.run("main", &[]).unwrap();
/// assert_eq!(result.as_int(), Some(42));
/// ```
pub fn run(&mut self, fn_name: &str, args: &[Value]) -> Result<Value, InterpError> {
    // ...
}
```

---

## Phase 5: Error Message Quality

### 5.1 Add Error Codes

Create `src/errors/codes.rs`:

```rust
/// Error codes for FORMA compiler diagnostics.
///
/// Format: EXXXX where X is category:
/// - E0XXX: Syntax/Parse errors
/// - E1XXX: Type errors
/// - E2XXX: Borrow errors
/// - E3XXX: MIR lowering errors
/// - E4XXX: Runtime errors

pub enum ErrorCode {
    // Parse errors
    E0001, // Unexpected token
    E0002, // Missing closing delimiter
    E0003, // Invalid literal
    E0004, // Expected expression
    E0005, // Expected type

    // Type errors
    E1001, // Type mismatch
    E1002, // Unknown type
    E1003, // Unknown variable
    E1004, // Wrong number of arguments
    E1005, // Cannot infer type
    E1006, // Recursive type without indirection

    // Borrow errors
    E2001, // Use after move
    E2002, // Cannot borrow as mutable
    E2003, // Cannot borrow while mutable borrow active
    E2004, // Cannot return reference to local
    E2005, // Cannot store reference in struct

    // Lowering errors
    E3001, // Unsupported feature
    E3002, // Invalid pattern

    // Runtime errors
    E4001, // Division by zero
    E4002, // Index out of bounds
    E4003, // Stack overflow
}

impl ErrorCode {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::E0001 => "E0001",
            Self::E1001 => "E1001",
            // ... etc
        }
    }

    pub fn explanation(&self) -> &'static str {
        match self {
            Self::E0001 => "The parser encountered a token it didn't expect at this position.",
            Self::E1001 => "The types of two expressions don't match where they should.",
            // ... etc
        }
    }
}
```

### 5.2 Improve Error Formatting

Update error display to include codes and suggestions:

```rust
// In ctx.error() calls, change from:
ctx.error(span, "type mismatch");

// To:
ctx.error_with_code(
    ErrorCode::E1001,
    span,
    format!("expected `{}`, found `{}`", expected, actual),
    Some(format!("try converting with `{}.to_{}()`", actual, expected)),
);
```

---

## Verification Checklist

After completing all phases, verify:

- [ ] `cargo build` succeeds with no warnings
- [ ] `cargo test` passes all tests
- [ ] `cargo clippy` has no warnings
- [ ] `cargo doc --no-deps` generates clean documentation
- [ ] These programs work correctly:

```bash
# Test 1: Basic types
echo 'f main() -> Int = 42' > /tmp/t1.forma
cargo run -- run /tmp/t1.forma  # Should print: 42

# Test 2: Option
echo 'f main() -> Int = m Some(42) { Some(x) => x, None => 0 }' > /tmp/t2.forma
cargo run -- run /tmp/t2.forma  # Should print: 42

# Test 3: User enum
cat > /tmp/t3.forma << 'EOF'
e Color = Red | Green | Blue
f to_num(c: Color) -> Int
    m c
        Red => 1
        Green => 2
        Blue => 3
f main() -> Int = to_num(Green)
EOF
cargo run -- run /tmp/t3.forma  # Should print: 2

# Test 4: Result
cat > /tmp/t4.forma << 'EOF'
f safe_div(a: Int, b: Int) -> Int!
    if b == 0 then Err("div by zero") else Ok(a / b)
f main() -> Int
    m safe_div(10, 2)
        Ok(x) => x
        Err(_) => -1
EOF
cargo run -- run /tmp/t4.forma  # Should print: 5
```

---

## Order of Operations

1. **Phase 1.1** - Fix `lower_type()` (blocks everything else)
2. **Phase 1.2-1.4** - Add enum support to MIR and interpreter
3. **Phase 2.1** - Integrate type/borrow checking into `run`
4. **Phase 2.2** - Add missing Ty variants
5. **Phase 3** - Write comprehensive tests
6. **Phase 4** - Add documentation
7. **Phase 5** - Improve error messages
8. **Verify** - Run the checklist above

After this is complete, proceed to SELF_HOSTING_PLAN_EXPANDED.md.
