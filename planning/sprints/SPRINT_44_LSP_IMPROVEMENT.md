# Sprint 44: LSP Improvement

## Context

The FORMA LSP (`src/lsp/mod.rs`, ~554 lines) currently implements 4 features: diagnostics, completions, hover, and go-to-definition. All operate on single-file scope with no caching. This sprint adds the highest-impact missing features: document formatting, document symbols, find references, and signature help. These are the features editors surface most visibly and that developers rely on minute-to-minute.

**Current state:** ~30-35% of a production LSP server.
**Target state:** ~55-60% — covering all features editors auto-invoke.

---

## Implementation Order

1. **44.1** — Document Formatting (wire existing `Formatter` into LSP)
2. **44.2** — Document Symbols (AST walk to produce symbol outline)
3. **44.3** — Signature Help (show parameter hints on `(` trigger)
4. **44.4** — Find References (scan tokens for name matches)
5. **44.5** — Improved Hover (show inferred types for user-defined symbols)
6. **44.6** — Tests + Verification

---

## Task 44.1: Document Formatting

The `Formatter` (`src/fmt/mod.rs`) already exists and works via the CLI `forma fmt` command. The LSP just needs to wire it in.

**File:** `src/lsp/mod.rs`

### Changes:

1. Add import at top:
```rust
use crate::fmt::Formatter;
```

2. Register capability in `initialize()` — add to `ServerCapabilities`:
```rust
document_formatting_provider: Some(OneOf::Left(true)),
```

3. Implement the `formatting` method on `LanguageServer`:
```rust
async fn formatting(
    &self,
    params: DocumentFormattingParams,
) -> Result<Option<Vec<TextEdit>>> {
    let uri = params.text_document.uri;

    let content = {
        let docs = self.documents.read().await;
        docs.get(&uri).map(|d| d.content.clone())
    };

    if let Some(content) = content {
        let scanner = Scanner::new(&content);
        let (tokens, lex_errors) = scanner.scan_all();

        if !lex_errors.is_empty() {
            return Ok(None); // Can't format with lex errors
        }

        let parser = crate::parser::Parser::new(&tokens);
        match parser.parse() {
            Ok(ast) => {
                let mut formatter = Formatter::new();
                let formatted = formatter.format(&ast);

                // Return a single edit replacing the entire document
                let line_count = content.lines().count() as u32;
                let last_line_len = content.lines().last().map_or(0, |l| l.len()) as u32;

                Ok(Some(vec![TextEdit {
                    range: Range {
                        start: Position { line: 0, character: 0 },
                        end: Position { line: line_count, character: last_line_len },
                    },
                    new_text: formatted,
                }]))
            }
            Err(_) => Ok(None), // Can't format with parse errors
        }
    } else {
        Ok(None)
    }
}
```

This is a whole-document replace edit. Editors will diff it against the current content and apply minimal changes visually.

---

## Task 44.2: Document Symbols

Provides the outline view (symbol tree) in editors. Walk the AST and emit `DocumentSymbol` for each top-level item.

**File:** `src/lsp/mod.rs`

### Changes:

1. Register capability in `initialize()`:
```rust
document_symbol_provider: Some(OneOf::Left(true)),
```

2. Implement the `document_symbol` method:
```rust
async fn document_symbol(
    &self,
    params: DocumentSymbolParams,
) -> Result<Option<DocumentSymbolResponse>> {
    let uri = params.text_document.uri;

    let content = {
        let docs = self.documents.read().await;
        docs.get(&uri).map(|d| d.content.clone())
    };

    if let Some(content) = content {
        let symbols = analyze_document_symbols(&content);
        Ok(Some(DocumentSymbolResponse::Flat(symbols)))
    } else {
        Ok(None)
    }
}
```

3. Add standalone `analyze_document_symbols()` function (for testability):
```rust
/// Extract document symbols from source content (extracted for testability).
pub fn analyze_document_symbols(content: &str) -> Vec<SymbolInformation> {
    let scanner = Scanner::new(content);
    let (tokens, lex_errors) = scanner.scan_all();

    if !lex_errors.is_empty() {
        return vec![];
    }

    let parser = crate::parser::Parser::new(&tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(_) => return vec![],
    };

    let mut symbols = Vec::new();

    for item in &ast.items {
        match &item.kind {
            ItemKind::Function(f) => {
                symbols.push(SymbolInformation {
                    name: f.name.name.clone(),
                    kind: SymbolKind::FUNCTION,
                    location: Location {
                        uri: Url::parse("file:///").unwrap(), // placeholder, caller should set
                        range: span_to_range(item.span),
                    },
                    tags: None,
                    deprecated: None,
                    container_name: None,
                });
            }
            ItemKind::Struct(s) => {
                symbols.push(SymbolInformation {
                    name: s.name.name.clone(),
                    kind: SymbolKind::STRUCT,
                    location: Location {
                        uri: Url::parse("file:///").unwrap(),
                        range: span_to_range(item.span),
                    },
                    tags: None,
                    deprecated: None,
                    container_name: None,
                });
            }
            ItemKind::Enum(e) => {
                symbols.push(SymbolInformation {
                    name: e.name.name.clone(),
                    kind: SymbolKind::ENUM,
                    location: Location {
                        uri: Url::parse("file:///").unwrap(),
                        range: span_to_range(item.span),
                    },
                    tags: None,
                    deprecated: None,
                    container_name: None,
                });
            }
            ItemKind::Trait(t) => {
                symbols.push(SymbolInformation {
                    name: t.name.name.clone(),
                    kind: SymbolKind::INTERFACE,
                    location: Location {
                        uri: Url::parse("file:///").unwrap(),
                        range: span_to_range(item.span),
                    },
                    tags: None,
                    deprecated: None,
                    container_name: None,
                });
            }
            ItemKind::Const(c) => {
                symbols.push(SymbolInformation {
                    name: c.name.name.clone(),
                    kind: SymbolKind::CONSTANT,
                    location: Location {
                        uri: Url::parse("file:///").unwrap(),
                        range: span_to_range(item.span),
                    },
                    tags: None,
                    deprecated: None,
                    container_name: None,
                });
            }
            ItemKind::TypeAlias(t) => {
                symbols.push(SymbolInformation {
                    name: t.name.name.clone(),
                    kind: SymbolKind::TYPE_PARAMETER,
                    location: Location {
                        uri: Url::parse("file:///").unwrap(),
                        range: span_to_range(item.span),
                    },
                    tags: None,
                    deprecated: None,
                    container_name: None,
                });
            }
            _ => {} // Impl, Use, Module — skip
        }
    }

    symbols
}
```

**Note:** We use `DocumentSymbolResponse::Flat(Vec<SymbolInformation>)` rather than the nested `DocumentSymbolResponse::Nested(Vec<DocumentSymbol>)` for simplicity. The flat variant is widely supported. The placeholder URI in `analyze_document_symbols` should be overwritten in the `document_symbol` handler — OR pass the URI into the function. Choose whichever is cleaner at implementation time. An alternative: pass `uri: &Url` as a parameter to `analyze_document_symbols`.

---

## Task 44.3: Signature Help

Show parameter names/types when the user types `(` after a function name.

**File:** `src/lsp/mod.rs`

### Changes:

1. Register capability in `initialize()`:
```rust
signature_help_provider: Some(SignatureHelpOptions {
    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
    retrigger_characters: None,
    work_done_progress_options: Default::default(),
}),
```

2. Implement the `signature_help` method:
```rust
async fn signature_help(
    &self,
    params: SignatureHelpParams,
) -> Result<Option<SignatureHelp>> {
    let uri = params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let content = {
        let docs = self.documents.read().await;
        docs.get(&uri).map(|d| d.content.clone())
    };

    if let Some(content) = content {
        Ok(get_signature_help(&content, position))
    } else {
        Ok(None)
    }
}
```

3. Add standalone function:
```rust
/// Get signature help for a function call at the given position.
fn get_signature_help(content: &str, position: Position) -> Option<SignatureHelp> {
    let scanner = Scanner::new(content);
    let (tokens, _) = scanner.scan_all();

    let line = position.line as usize + 1;
    let col = position.character as usize + 1;

    // Walk backwards from cursor to find the function name before the `(`
    // Strategy: find the nearest `(` token before/at cursor, then look for
    // the Ident immediately before it.
    let mut fn_name = None;
    let mut active_param: u32 = 0;

    for (i, token) in tokens.iter().enumerate() {
        // Count commas between `(` and cursor to determine active parameter
        if token.span.line == line && token.span.column <= col {
            if let crate::lexer::TokenKind::LParen = &token.kind {
                // Look back for function name
                if i > 0 {
                    if let crate::lexer::TokenKind::Ident(name) = &tokens[i - 1].kind {
                        fn_name = Some(name.clone());
                        active_param = 0;
                    }
                }
            }
            if let crate::lexer::TokenKind::Comma = &token.kind {
                if fn_name.is_some() {
                    active_param += 1;
                }
            }
        }
    }

    let name = fn_name?;

    // Look up signature from builtins
    let info = get_builtin_info(&name)?;
    // info is like "print(value: T) -> ()\nPrint a value to stdout"
    let sig_line = info.lines().next().unwrap_or(&info);

    Some(SignatureHelp {
        signatures: vec![SignatureInformation {
            label: sig_line.to_string(),
            documentation: Some(Documentation::String(info.clone())),
            parameters: None, // Could parse params from sig_line for highlighting
            active_parameter: Some(active_param),
        }],
        active_signature: Some(0),
        active_parameter: Some(active_param),
    })
}
```

**Limitation:** This only works for builtins (since we already have `get_builtin_info`). Extending to user-defined functions would require parsing the AST and extracting parameter info from function definitions — a good follow-up for a later sprint. For now, builtin signature help alone is very useful since those are the functions users need the most help with.

---

## Task 44.4: Find References

Find all occurrences of an identifier in the current file. Token-level scan (not semantic — doesn't distinguish between same-named identifiers in different scopes).

**File:** `src/lsp/mod.rs`

### Changes:

1. Register capability in `initialize()`:
```rust
references_provider: Some(OneOf::Left(true)),
```

2. Implement the `references` method:
```rust
async fn references(
    &self,
    params: ReferenceParams,
) -> Result<Option<Vec<Location>>> {
    let uri = params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;

    let content = {
        let docs = self.documents.read().await;
        docs.get(&uri).map(|d| d.content.clone())
    };

    if let Some(content) = content {
        let scanner = Scanner::new(&content);
        let (tokens, _) = scanner.scan_all();

        let line = position.line as usize + 1;
        let col = position.character as usize + 1;

        // Find the identifier at cursor
        let target_name = tokens.iter().find_map(|token| {
            let token_end = token.span.column + (token.span.end - token.span.start);
            if token.span.line == line && token.span.column <= col && col <= token_end {
                if let crate::lexer::TokenKind::Ident(name) = &token.kind {
                    Some(name.clone())
                } else {
                    None
                }
            } else {
                None
            }
        });

        if let Some(name) = target_name {
            // Find all tokens with the same identifier name
            let locations: Vec<Location> = tokens
                .iter()
                .filter_map(|token| {
                    if let crate::lexer::TokenKind::Ident(n) = &token.kind {
                        if n == &name {
                            Some(Location {
                                uri: uri.clone(),
                                range: span_to_range(token.span),
                            })
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .collect();

            if locations.is_empty() {
                Ok(None)
            } else {
                Ok(Some(locations))
            }
        } else {
            Ok(None)
        }
    } else {
        Ok(None)
    }
}
```

**Limitation:** This is token-level, not scope-aware. A variable `x` in function `foo` and a different `x` in function `bar` will both be returned. Scope-aware references would require a symbol table pass — good follow-up for a later sprint. Even token-level find-references is useful for rename exploration and navigation.

---

## Task 44.5: Improved Hover (Inferred Types)

Currently hover only shows keyword descriptions and builtin signatures. Enhance it to show inferred types for user-defined symbols by leveraging the type checker.

**File:** `src/lsp/mod.rs`

### Changes:

Modify `get_hover()` to fall through to type checker when the token is an `Ident` that isn't a builtin:

Replace the current `Ident` arm in `get_hover`:
```rust
crate::lexer::TokenKind::Ident(name) => {
    get_builtin_info(name).unwrap_or_else(|| format!("identifier: {}", name))
}
```

With:
```rust
crate::lexer::TokenKind::Ident(name) => {
    if let Some(info) = get_builtin_info(name) {
        info
    } else {
        // Try to get type information from the type checker
        get_symbol_type_info(content, name)
            .unwrap_or_else(|| format!("identifier: {}", name))
    }
}
```

Add helper function:
```rust
/// Try to get type information for a symbol from the type checker.
fn get_symbol_type_info(content: &str, name: &str) -> Option<String> {
    let scanner = Scanner::new(content);
    let (tokens, lex_errors) = scanner.scan_all();
    if !lex_errors.is_empty() {
        return None;
    }

    let parser = crate::parser::Parser::new(&tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(_) => return None,
    };

    let mut type_checker = TypeChecker::new();
    let _ = type_checker.check(&ast); // Ignore errors, we just want the env

    // Look up the symbol's type in the environment
    if let Some(scheme) = type_checker.env().get(name) {
        let (def_span, def_kind) = type_checker
            .get_definition_location(name)
            .unwrap_or_default();
        let kind_str = match def_kind {
            crate::types::DefinitionKind::Function => "function",
            crate::types::DefinitionKind::Struct => "struct",
            crate::types::DefinitionKind::Enum => "enum",
            crate::types::DefinitionKind::Trait => "trait",
            crate::types::DefinitionKind::TypeAlias => "type alias",
            crate::types::DefinitionKind::Variable => "variable",
            crate::types::DefinitionKind::Parameter => "parameter",
            crate::types::DefinitionKind::EnumVariant => "variant",
        };
        Some(format!("{} {}: {}", kind_str, name, scheme.ty))
    } else {
        None
    }
}
```

**Note:** This re-parses and re-type-checks on each hover. This is acceptable for now since FORMA files are small. A caching layer (store last-good AST + TypeChecker result per document) would be a good optimization for a later sprint.

**Implementation detail:** Check that `TypeChecker::env()` returns the type environment (it does — `src/types/checker.rs:167`). Check that `TypeScheme` has a displayable `ty` field. Verify `DefinitionKind` has a `Default` impl or use `Option` instead of `unwrap_or_default`. Adjust at implementation time based on what compiles.

---

## Task 44.6: Tests + Verification

### New tests in `src/lsp/mod.rs` `#[cfg(test)] mod tests`:

1. **`test_document_symbols_function_and_struct`** — Parse source with a function and struct, verify `analyze_document_symbols` returns both with correct kinds.

2. **`test_document_symbols_empty`** — Empty/invalid source returns empty vec.

3. **`test_completions_after_arrow`** — Verify type completions appear when prev token is `->`.

4. **`test_completions_after_dot`** — Verify method completions appear after `.`.

5. **`test_diagnostics_type_error`** — Source with a type error produces TYPE diagnostic.

### Verification checklist:

1. `cargo build` — clean
2. `cargo test --all` — all existing + new tests pass
3. `cargo clippy --all-targets -- -D warnings` — zero warnings
4. `cargo clippy --all-features --all-targets -- -D warnings` — zero warnings
5. `cargo fmt --all -- --check` — clean

---

## Critical Files

| File | Changes |
|------|---------|
| `src/lsp/mod.rs` | All 5 feature tasks (44.1–44.5) + tests (44.6) |

Only one file changes. All new features are additive — no modifications to existing methods except the `Ident` arm in `get_hover()` (44.5) and the `ServerCapabilities` struct in `initialize()`.

## Dependencies

No new crate dependencies. All features use existing infrastructure:
- `crate::fmt::Formatter` (44.1)
- `crate::parser::ItemKind` (44.2)
- `crate::lexer::TokenKind` (44.3, 44.4)
- `crate::types::TypeChecker` + `env()` + `get_definition_location()` (44.5)

## Risks

- **Performance:** Tasks 44.1, 44.2, and 44.5 each re-lex/re-parse the document. For small FORMA files (<1000 lines) this is fine. For larger files, a caching layer (store last-good parse result per document) would help — defer to a future sprint.
- **Signature help scope:** Only covers builtins. User-defined function signatures require AST lookup — straightforward but deferred.
- **Find references scope:** Token-level, not scope-aware. Good enough for single-file editing but could produce false positives for common names.

## Future Work (not in this sprint)

- Incremental text sync (INCREMENTAL instead of FULL)
- Per-document AST/type-check cache
- Cross-file go-to-definition and find-references via module loader
- Code actions (quick fixes for common type errors)
- Rename symbol (scope-aware)
- Semantic tokens (syntax highlighting via LSP)
- User-defined function signature help
- Workspace symbols
- Inlay hints for inferred types
