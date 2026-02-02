# Sprint 15: v1.1 Prescriptive Implementation Plan

**Date:** January 26, 2026
**CRITICAL:** Follow these specifications EXACTLY. Do not improvise or create alternative solutions.

---

## Task 15.1: Multi-Error Reporting

**Files to modify:**
- `src/parser/parser.rs`
- `src/main.rs`

### Step 1: Add errors field to Parser struct

**Location:** `src/parser/parser.rs` line 13

**CURRENT:**
```rust
pub struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}
```

**CHANGE TO:**
```rust
pub struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
    errors: Vec<crate::errors::CompileError>,
}
```

### Step 2: Update Parser constructor

**Location:** `src/parser/parser.rs` lines 16-18

**CURRENT:**
```rust
pub fn new(tokens: &'a [Token]) -> Self {
    Self { tokens, pos: 0 }
}
```

**CHANGE TO:**
```rust
pub fn new(tokens: &'a [Token]) -> Self {
    Self {
        tokens,
        pos: 0,
        errors: Vec::new(),
    }
}
```

### Step 3: Change parse() return type and error handling

**Location:** `src/parser/parser.rs` lines 23-59

**CURRENT:** Returns `Result<SourceFile>` and stores only first error

**REPLACE ENTIRE FUNCTION with:**
```rust
pub fn parse(mut self) -> std::result::Result<SourceFile, Vec<crate::errors::CompileError>> {
    let start = self.current_span();
    let mut items = Vec::new();

    while !self.at_end() {
        while self.check(TokenKind::Newline) {
            self.advance();
        }
        if self.at_end() {
            break;
        }

        match self.parse_item() {
            Ok(item) => items.push(item),
            Err(e) => {
                self.errors.push(e);
                self.synchronize();
            }
        }
    }

    if !self.errors.is_empty() {
        return Err(self.errors);
    }

    let end = self.previous_span();
    Ok(SourceFile {
        items,
        span: start.merge(end),
    })
}
```

### Step 4: Update main.rs parse error handling

**Location:** `src/main.rs` - Find ALL locations where `parser.parse()` is called and update error handling.

**Pattern to find:** `Err(e) =>` after `parser.parse()`

**CHANGE FROM:**
```rust
Err(e) => {
    // handles single error
}
```

**CHANGE TO:**
```rust
Err(errors) => {
    for error in &errors {
        match error_format {
            ErrorFormat::Human => ctx.error(error.span(), &format!("{}", error)),
            ErrorFormat::Json => {
                json_errors.push(span_to_json_error(
                    &filename,
                    error.span(),
                    "PARSE",
                    &format!("{}", error),
                    error.help(),
                ));
            }
        }
    }
    if error_format == ErrorFormat::Json {
        output_json_errors(json_errors, None);
    }
    return Err(format!("{} parse error(s)", errors.len()));
}
```

### Acceptance Test:
Create `tests/forma/test_multi_error.forma` with intentional syntax errors on multiple lines. Verify all errors are reported.

---

## Task 15.2: REPL Type Display

**Files to modify:**
- `src/main.rs`

### Step 1: Update repl_type_of function

**Location:** `src/main.rs` lines 1977-2018

**FIND this code block (around line 2007-2010):**
```rust
Ok(_) => {
    // Successfully type-checked - expression is well-typed
    // For a full implementation, we'd need to extract the inferred type from the inference engine
    println!("Expression is well-typed");
}
```

**REPLACE WITH:**
```rust
Ok(_) => {
    if let Some(expr_type) = type_checker.type_of("__result__") {
        let finalized = type_checker.finalize(&expr_type);
        println!("{}", finalized);
    } else {
        println!("Expression is well-typed");
    }
}
```

### Acceptance Test:
```
forma> :type [1, 2, 3]
Vec[Int]

forma> :type |x| x + 1
(Int) -> Int

forma> :type "hello"
Str
```

---

## Task 15.3: LSP Go-to-Definition

**Files to modify:**
- `src/types/inference.rs`
- `src/types/checker.rs`
- `src/lsp/mod.rs`

### Step 1: Add DefinitionKind enum

**Location:** `src/types/checker.rs` after line 10 (before TypeChecker struct)

**ADD:**
```rust
/// Kinds of definitions that can be jumped to
#[derive(Debug, Clone, Copy)]
pub enum DefinitionKind {
    Function,
    Struct,
    Enum,
    Trait,
    TypeAlias,
    Variable,
    Parameter,
    EnumVariant,
}
```

### Step 2: Add symbol_locations to InferenceEngine

**Location:** `src/types/inference.rs` find the InferenceEngine struct (around line 2813)

**ADD this field:**
```rust
pub struct InferenceEngine {
    // ... existing fields ...
    /// Track where symbols are defined for LSP
    symbol_locations: HashMap<String, (Span, DefinitionKind)>,
}
```

**UPDATE the InferenceEngine::new() to initialize:**
```rust
symbol_locations: HashMap::new(),
```

### Step 3: Record function definitions in collect_function_sig()

**Location:** `src/types/inference.rs` in `collect_function_sig()` function, after `self.env.insert()` calls

**ADD after each function is registered:**
```rust
self.symbol_locations.insert(
    f.name.name.clone(),
    (f.name.span, DefinitionKind::Function),
);
```

### Step 4: Record struct/enum definitions in collect_type_def()

**Location:** `src/types/inference.rs` in `collect_type_def()` function

**For structs, ADD after struct is registered:**
```rust
self.symbol_locations.insert(
    struct_name.clone(),
    (s.name.span, DefinitionKind::Struct),
);
```

**For enums, ADD after enum is registered:**
```rust
self.symbol_locations.insert(
    enum_name.clone(),
    (e.name.span, DefinitionKind::Enum),
);
```

### Step 5: Add accessor methods to InferenceEngine

**Location:** `src/types/inference.rs` in the `impl InferenceEngine` block

**ADD:**
```rust
pub fn get_symbol_location(&self, name: &str) -> Option<(Span, DefinitionKind)> {
    self.symbol_locations.get(name).copied()
}
```

### Step 6: Add accessor to TypeChecker

**Location:** `src/types/checker.rs` in the `impl TypeChecker` block

**ADD:**
```rust
pub fn get_definition_location(&self, name: &str) -> Option<(Span, DefinitionKind)> {
    self.engine.get_symbol_location(name)
}
```

### Step 7: Implement goto_definition in LSP

**Location:** `src/lsp/mod.rs` lines 384-391

**REPLACE the entire function with:**
```rust
async fn goto_definition(
    &self,
    params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>> {
    let uri = params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let content = {
        let docs = self.documents.read().await;
        docs.get(&uri).map(|d| d.content.clone())
    };

    if let Some(content) = content {
        let scanner = Scanner::new(&content);
        let (tokens, _) = scanner.scan_all();

        let line = position.line as usize + 1;
        let col = position.character as usize + 1;

        // Find identifier at cursor
        let identifier_name = tokens.iter()
            .find(|token| {
                let token_end = token.span.column + (token.span.end - token.span.start);
                token.span.line == line && token.span.column <= col && col <= token_end
            })
            .and_then(|token| {
                if let crate::lexer::TokenKind::Ident(name) = &token.kind {
                    Some(name.clone())
                } else {
                    None
                }
            });

        if let Some(name) = identifier_name {
            let parser = crate::parser::Parser::new(&tokens);
            if let Ok(ast) = parser.parse() {
                let mut type_checker = crate::types::TypeChecker::new();
                if type_checker.check(&ast).is_ok() {
                    if let Some((def_span, _)) = type_checker.get_definition_location(&name) {
                        let location = Location {
                            uri: uri.clone(),
                            range: span_to_range(def_span),
                        };
                        return Ok(Some(GotoDefinitionResponse::Scalar(location)));
                    }
                }
            }
        }
    }

    Ok(None)
}
```

### Step 8: Add import for DefinitionKind in lsp/mod.rs

**Location:** `src/lsp/mod.rs` at top of file

**ADD:**
```rust
use crate::types::checker::DefinitionKind;
```

---

## Task 15.4: Trait Implementation Checking

**Files to modify:**
- `src/types/inference.rs`

### Step 1: Add traits storage to TypeEnv

**Location:** `src/types/inference.rs` find TypeEnv struct (around line 63)

**ADD this field:**
```rust
/// Trait definitions
traits: HashMap<String, TraitInfo>,
```

**ADD this struct before TypeEnv:**
```rust
#[derive(Debug, Clone)]
pub struct TraitInfo {
    pub name: String,
    pub type_params: Vec<String>,
    pub methods: Vec<TraitMethodInfo>,
}

#[derive(Debug, Clone)]
pub struct TraitMethodInfo {
    pub name: String,
    pub params: Vec<Ty>,
    pub return_type: Ty,
    pub has_default: bool,
}
```

### Step 2: Initialize traits HashMap in TypeEnv::new()

**Location:** Find TypeEnv::new() and add:
```rust
traits: HashMap::new(),
```

### Step 3: Add TypeEnv helper methods

**Location:** In `impl TypeEnv` block

**ADD:**
```rust
pub fn insert_trait(&mut self, name: String, info: TraitInfo) {
    self.traits.insert(name, info);
}

pub fn get_trait(&self, name: &str) -> Option<&TraitInfo> {
    self.traits.get(name)
}
```

### Step 4: Collect trait definitions in collect_type_def()

**Location:** `src/types/inference.rs` in `collect_type_def()`, add a case for ItemKind::Trait

**ADD after the existing match arms (before `_ => {}`):**
```rust
ItemKind::Trait(t) => {
    let trait_name = t.name.name.clone();
    let type_params = self.get_type_params(&t.generics);

    let old_type_params = std::mem::take(&mut self.type_params);
    self.type_params = self.setup_type_params(&t.generics);

    let mut methods = Vec::new();
    for trait_item in &t.items {
        if let crate::parser::TraitItem::Function(f) = trait_item {
            let param_types: Vec<Ty> = f.params.iter()
                .filter(|p| p.name.name != "self")
                .filter_map(|p| self.ast_type_to_ty(&p.ty).ok())
                .collect();

            let return_type = f.return_type.as_ref()
                .and_then(|ty| self.ast_type_to_ty(ty).ok())
                .unwrap_or(Ty::Unit);

            methods.push(TraitMethodInfo {
                name: f.name.name.clone(),
                params: param_types,
                return_type,
                has_default: f.body.is_some(),
            });
        }
    }

    self.env.insert_trait(trait_name.clone(), TraitInfo {
        name: trait_name,
        type_params,
        methods,
    });

    self.type_params = old_type_params;
}
```

### Step 5: Validate trait implementations in check_item()

**Location:** `src/types/inference.rs` in `check_item()`, inside the `ItemKind::Impl` handling

**ADD after impl items are checked, before the closing brace:**
```rust
// Validate trait implementation
if let Some(trait_type) = &i.trait_ {
    let trait_name = match &trait_type.kind {
        crate::parser::TypeKind::Path(p) => p.segments.last()
            .map(|s| s.name.name.clone())
            .unwrap_or_default(),
        _ => String::new(),
    };

    if let Some(trait_info) = self.env.get_trait(&trait_name) {
        let mut impl_methods: std::collections::HashSet<String> = std::collections::HashSet::new();
        for impl_item in &i.items {
            if let crate::parser::ImplItem::Function(f) = impl_item {
                impl_methods.insert(f.name.name.clone());
            }
        }

        // Check all required methods are implemented
        for method in &trait_info.methods {
            if !method.has_default && !impl_methods.contains(&method.name) {
                return Err(TypeError::new(
                    format!(
                        "missing method '{}' required by trait '{}'",
                        method.name, trait_name
                    ),
                    i.span,
                ));
            }
        }

        // Validate method signatures
        for impl_item in &i.items {
            if let crate::parser::ImplItem::Function(f) = impl_item {
                if let Some(trait_method) = trait_info.methods.iter()
                    .find(|m| m.name == f.name.name)
                {
                    let impl_params: Vec<_> = f.params.iter()
                        .filter(|p| p.name.name != "self")
                        .collect();

                    if impl_params.len() != trait_method.params.len() {
                        return Err(TypeError::new(
                            format!(
                                "method '{}' has {} parameter(s), trait requires {}",
                                f.name.name, impl_params.len(), trait_method.params.len()
                            ),
                            f.span,
                        ));
                    }
                }
            }
        }
    }
}
```

---

## Task 15.5: Enum Pattern Validation

**Files to modify:**
- `src/types/inference.rs`

### Step 1: Update check_pattern() for enum validation

**Location:** `src/types/inference.rs` in `check_pattern()`, find the `PatternKind::Struct` handling

**The current code only checks for TypeDef::Struct. ADD a check for TypeDef::Enum AFTER the struct handling:**

**FIND the section that starts with:**
```rust
if let Some(TypeDef::Struct { ... }) = struct_def {
```

**ADD AFTER that entire if block (before the else fallback):**
```rust
} else if let Some(TypeDef::Enum { type_params, variants }) = struct_def {
    // This is an enum pattern like Some(x) or Color::Red
    let enum_ty = if type_params.is_empty() {
        Ty::Named(TypeId::new(&struct_name), vec![])
    } else {
        let type_args: Vec<Ty> = type_params.iter()
            .map(|_| Ty::fresh_var())
            .collect();
        Ty::Named(TypeId::new(&struct_name), type_args)
    };
    self.unifier.unify(&enum_ty, ty, pattern.span)?;

    // Get variant name from pattern
    let variant_name = if path.segments.len() >= 2 {
        path.segments.last().map(|s| s.name.name.clone()).unwrap_or_default()
    } else {
        fields.first().map(|f| f.name.name.clone()).unwrap_or_default()
    };

    // Find variant in enum definition
    let variant = variants.iter()
        .find(|(name, _)| name == &variant_name);

    if let Some((_, field_types)) = variant {
        // Validate field count
        let pattern_field_count = fields.len();
        if pattern_field_count != field_types.len() {
            return Err(TypeError::new(
                format!(
                    "variant '{}::{}' has {} field(s), pattern has {}",
                    struct_name, variant_name, field_types.len(), pattern_field_count
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
        return Err(TypeError::new(
            format!(
                "enum '{}' has no variant '{}'. Available: {}",
                struct_name, variant_name, available.join(", ")
            ),
            pattern.span,
        ));
    }
```

---

## Task 15.6: Formatter Completeness

**Files to modify:**
- `src/fmt/mod.rs`

### Step 1: Fix format_type() catch-all (line 247)

**FIND:**
```rust
_ => self.write("..."),
```

**REPLACE WITH all missing TypeKind handlers:**
```rust
TypeKind::Map(key_ty, val_ty) => {
    self.write("{");
    self.format_type(key_ty);
    self.write(": ");
    self.format_type(val_ty);
    self.write("}");
}
TypeKind::Set(elem_ty) => {
    self.write("{");
    self.format_type(elem_ty);
    self.write("}");
}
TypeKind::Array(elem_ty, size_expr) => {
    self.write("[");
    self.format_type(elem_ty);
    self.write("; ");
    self.format_expr(size_expr);
    self.write("]");
}
TypeKind::Fn(params, ret) => {
    self.write("(");
    for (i, param) in params.iter().enumerate() {
        if i > 0 { self.write(", "); }
        self.format_type(param);
    }
    self.write(") -> ");
    self.format_type(ret);
}
TypeKind::Ptr(inner, is_mut) => {
    self.write("*");
    if *is_mut { self.write("mut "); }
    self.format_type(inner);
}
_ => self.write("?"),
```

### Step 2: Fix format_binop() catch-all (line 407)

**FIND:**
```rust
_ => "?",
```

**REPLACE WITH:**
```rust
BinOp::BitAnd => "&",
BinOp::BitOr => "|",
BinOp::BitXor => "^",
BinOp::Shl => "<<",
BinOp::Shr => ">>",
```

### Step 3: Fix format_pattern() catch-all (line 443)

**FIND:**
```rust
_ => self.write("..."),
```

**REPLACE WITH all missing PatternKind handlers:**
```rust
PatternKind::List(pats, rest) => {
    self.write("[");
    for (i, p) in pats.iter().enumerate() {
        if i > 0 { self.write(", "); }
        self.format_pattern(p);
    }
    if let Some(rest_pat) = rest {
        if !pats.is_empty() { self.write(", "); }
        self.write("..");
        self.format_pattern(rest_pat);
    }
    self.write("]");
}
PatternKind::Struct(path, fields, _) => {
    for (i, seg) in path.segments.iter().enumerate() {
        if i > 0 { self.write("::"); }
        self.write(&seg.name.name);
    }
    self.write(" { ");
    for (i, field) in fields.iter().enumerate() {
        if i > 0 { self.write(", "); }
        self.write(&field.name.name);
        if let Some(pat) = &field.pattern {
            self.write(": ");
            self.format_pattern(pat);
        }
    }
    self.write(" }");
}
PatternKind::Or(pats) => {
    for (i, p) in pats.iter().enumerate() {
        if i > 0 { self.write(" | "); }
        self.format_pattern(p);
    }
}
PatternKind::Range(start, end, inclusive) => {
    if let Some(s) = start { self.format_pattern(s); }
    self.write(if *inclusive { "..=" } else { ".." });
    if let Some(e) = end { self.format_pattern(e); }
}
PatternKind::Ref(pat, is_mut) => {
    self.write("&");
    if *is_mut { self.write("mut "); }
    self.format_pattern(pat);
}
PatternKind::Rest => self.write(".."),
```

### Step 4: Fix format_item() Use statement (lines 76-79)

**FIND:**
```rust
ItemKind::Use(_) => {
    self.write_indent();
    self.write("us ...");
    self.newline();
}
```

**REPLACE WITH:**
```rust
ItemKind::Use(use_stmt) => {
    self.write_indent();
    self.write("us ");
    self.format_use_tree(&use_stmt.tree);
    self.newline();
}
```

**ADD this new helper method:**
```rust
fn format_use_tree(&mut self, tree: &UseTree) {
    match tree {
        UseTree::Path(segments, sub) => {
            for (i, seg) in segments.iter().enumerate() {
                if i > 0 { self.write("."); }
                self.write(&seg.name);
            }
            if let Some(sub_tree) = sub {
                self.write(".");
                self.format_use_tree(sub_tree);
            }
        }
        UseTree::Rename(segments, alias) => {
            for (i, seg) in segments.iter().enumerate() {
                if i > 0 { self.write("."); }
                self.write(&seg.name);
            }
            self.write(" -> ");
            self.write(&alias.name);
        }
        UseTree::Group(trees) => {
            self.write("{");
            for (i, t) in trees.iter().enumerate() {
                if i > 0 { self.write(", "); }
                self.format_use_tree(t);
            }
            self.write("}");
        }
        UseTree::Glob => self.write("*"),
    }
}
```

### Step 5: Add missing ExprKind handlers in format_expr() (line 369)

This is a large change. The catch-all `_ => self.write("...")` should be replaced with handlers for ALL expression kinds. Key ones:

```rust
ExprKind::Path(path) => {
    for (i, seg) in path.segments.iter().enumerate() {
        if i > 0 { self.write("."); }
        self.write(&seg.name.name);
    }
}
ExprKind::MethodCall(obj, method, args) => {
    self.format_expr(obj);
    self.write(".");
    self.write(&method.name);
    self.write("(");
    for (i, arg) in args.iter().enumerate() {
        if i > 0 { self.write(", "); }
        self.format_expr(&arg.value);
    }
    self.write(")");
}
ExprKind::Field(obj, field) => {
    self.format_expr(obj);
    self.write(".");
    self.write(&field.name);
}
ExprKind::Index(obj, idx) => {
    self.format_expr(obj);
    self.write("[");
    self.format_expr(idx);
    self.write("]");
}
ExprKind::Closure(closure) => {
    self.write("|");
    for (i, param) in closure.params.iter().enumerate() {
        if i > 0 { self.write(", "); }
        self.write(&param.name.name);
    }
    self.write("| ");
    self.format_expr(&closure.body);
}
ExprKind::Try(expr) => {
    self.format_expr(expr);
    self.write("?");
}
ExprKind::Await(expr) => {
    self.format_expr(expr);
    self.write(".await");
}
ExprKind::Spawn(expr) => {
    self.write("sp ");
    self.format_expr(expr);
}
// Add all other ExprKind variants...
_ => self.write("?"),
```

---

## Task 15.7: Struct Update Syntax

**Files to modify:**
- `src/mir/lower.rs`

### Step 1: Implement struct update in lower_expr()

**Location:** `src/mir/lower.rs` lines 962-965

**FIND:**
```rust
// Handle base (struct update syntax)
if let Some(_base_expr) = base {
    // TODO: handle struct update
}
```

**REPLACE WITH:**
```rust
// Handle base (struct update syntax)
if let Some(base_expr) = base {
    // Lower the base expression
    let base_op = self.lower_expr(base_expr)?;

    // Get struct field names from type info
    if let Some(struct_fields) = self.get_struct_fields(&name) {
        let existing_field_names: std::collections::HashSet<_> = mir_fields.iter()
            .map(|(n, _)| n.clone())
            .collect();

        // Copy fields from base that aren't explicitly set
        for (field_name, field_ty) in struct_fields {
            if !existing_field_names.contains(&field_name) {
                let field_local = self.new_temp(field_ty);
                self.emit(StatementKind::Assign(
                    field_local,
                    Rvalue::Field(base_op.clone(), field_name.clone()),
                ));
                mir_fields.push((field_name, Operand::Local(field_local)));
            }
        }
    }
}
```

### Step 2: Add get_struct_fields helper

**Location:** `src/mir/lower.rs` add this method to the impl block

**ADD:**
```rust
fn get_struct_fields(&self, struct_name: &str) -> Option<Vec<(String, Ty)>> {
    // Look up struct definition in var_types or type registry
    // For now, return None and rely on type checker having validated
    // In a full implementation, this would query the struct registry
    None
}
```

**Note:** This is a simplified implementation. For full functionality, the lowerer needs access to struct definitions from the type checker.

---

## Verification

After completing all tasks, run:

```bash
cargo test
cargo run -- run tests/forma/test_multi_error.forma
cargo run -- repl  # Test :type command
scripts/verify_v1.sh
```

---

## CRITICAL REMINDERS

1. **DO NOT** use FNV-1a or any hash-based solutions for enum discriminants - we already fixed this with index-based registry
2. **DO NOT** create alternative solutions - follow these specifications EXACTLY
3. **DO NOT** skip any steps - each step builds on previous ones
4. **DO** run `cargo test` after each task to catch regressions
5. **DO** commit after each successfully completed task

---

*"Precision in specification prevents improvisation in implementation."*
