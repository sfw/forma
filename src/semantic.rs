//! Stable semantic identities for editor and compiler queries.
//!
//! This lightweight index resolves identifiers through lexical scopes.  It is
//! deliberately independent of presentation-layer spelling searches, so two
//! shadowed bindings with the same name remain distinct symbols.

use std::collections::HashMap;

use crate::lexer::{Span, Token, TokenKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefinitionId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Function,
    Type,
    Module,
    Parameter,
    Local,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Definition {
    pub id: DefinitionId,
    pub name: String,
    pub span: Span,
    pub kind: SymbolKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Reference {
    pub span: Span,
    pub definition: DefinitionId,
}

#[derive(Debug, Clone, Default)]
pub struct SemanticIndex {
    definitions: Vec<Definition>,
    references: Vec<Reference>,
}

impl SemanticIndex {
    pub fn from_tokens(tokens: &[Token]) -> Self {
        let mut index = Self::default();
        let mut declaration_ids = HashMap::new();
        let mut global = HashMap::new();

        // Collect file-level declarations first, making forward references
        // deterministic and independent of source order.
        for pair in tokens.windows(2) {
            let kind = declaration_kind(&pair[0]);
            if let (Some(kind), TokenKind::Ident(name)) = (kind, &pair[1].kind) {
                let id = index.define(name.clone(), pair[1].span, kind);
                declaration_ids.insert(pair[1].span.start, id);
                global.insert(name.clone(), id);
            }
        }

        let mut scopes = vec![global];
        let mut pending_params: Vec<(String, DefinitionId)> = Vec::new();
        let mut inline_params: HashMap<String, DefinitionId> = HashMap::new();
        let mut in_function_params = false;
        let mut paren_depth = 0usize;
        let mut expect_function_name = false;

        for (position, token) in tokens.iter().enumerate() {
            match token.kind {
                TokenKind::F => {
                    expect_function_name = true;
                    continue;
                }
                TokenKind::Indent => {
                    let mut scope = HashMap::new();
                    for (name, id) in pending_params.drain(..) {
                        scope.insert(name, id);
                    }
                    inline_params.clear();
                    scopes.push(scope);
                    continue;
                }
                TokenKind::Dedent => {
                    if scopes.len() > 1 {
                        scopes.pop();
                    }
                    continue;
                }
                TokenKind::Newline => {
                    inline_params.clear();
                    continue;
                }
                TokenKind::LParen if expect_function_name => {
                    in_function_params = true;
                    paren_depth = 1;
                    expect_function_name = false;
                    continue;
                }
                TokenKind::LParen if in_function_params => {
                    paren_depth += 1;
                    continue;
                }
                TokenKind::RParen if in_function_params => {
                    paren_depth = paren_depth.saturating_sub(1);
                    if paren_depth == 0 {
                        in_function_params = false;
                    }
                    continue;
                }
                _ => {}
            }
            if declaration_kind(token) == Some(SymbolKind::Function) {
                expect_function_name = true;
                continue;
            }

            let TokenKind::Ident(name) = &token.kind else {
                continue;
            };

            if declaration_ids.contains_key(&token.span.start) {
                // The function name is followed by its parameter list.
                if matches!(
                    index.definition_at(token.span.start).map(|d| d.kind),
                    Some(SymbolKind::Function)
                ) {
                    expect_function_name = true;
                }
                continue;
            }

            let next = tokens.get(position + 1).map(|next| &next.kind);
            let previous = position
                .checked_sub(1)
                .and_then(|p| tokens.get(p))
                .map(|t| &t.kind);
            let is_parameter = in_function_params && matches!(next, Some(TokenKind::Colon));
            if is_parameter {
                let id = index.define(name.clone(), token.span, SymbolKind::Parameter);
                pending_params.push((name.clone(), id));
                inline_params.insert(name.clone(), id);
                continue;
            }

            let binding_operator = matches!(next, Some(TokenKind::Eq | TokenKind::ColonEq))
                && matches!(
                    previous,
                    None | Some(
                        TokenKind::Newline
                            | TokenKind::Indent
                            | TokenKind::Dedent
                            | TokenKind::Semicolon
                    )
                );
            let member_name = matches!(previous, Some(TokenKind::Dot | TokenKind::ColonColon));
            if binding_operator && !member_name {
                let existing = resolve(name, &inline_params, &scopes);
                let updates_existing =
                    matches!(next, Some(TokenKind::ColonEq)) && existing.is_some();
                if updates_existing {
                    index.references.push(Reference {
                        span: token.span,
                        definition: existing.expect("checked above"),
                    });
                } else {
                    let id = index.define(name.clone(), token.span, SymbolKind::Local);
                    scopes
                        .last_mut()
                        .expect("semantic index always has a global scope")
                        .insert(name.clone(), id);
                }
                continue;
            }

            if member_name {
                continue;
            }
            if let Some(definition) = resolve(name, &inline_params, &scopes) {
                index.references.push(Reference {
                    span: token.span,
                    definition,
                });
            }
        }
        index
    }

    pub fn definitions(&self) -> &[Definition] {
        &self.definitions
    }

    pub fn references(&self) -> &[Reference] {
        &self.references
    }

    pub fn definition(&self, id: DefinitionId) -> Option<&Definition> {
        self.definitions.get(id.0 as usize)
    }

    pub fn definition_at(&self, offset: usize) -> Option<&Definition> {
        self.definitions
            .iter()
            .find(|definition| contains(definition.span, offset))
            .or_else(|| {
                self.references
                    .iter()
                    .find(|reference| contains(reference.span, offset))
                    .and_then(|reference| self.definition(reference.definition))
            })
    }

    pub fn references_to(&self, id: DefinitionId, include_declaration: bool) -> Vec<Span> {
        let mut spans = Vec::new();
        if include_declaration && let Some(definition) = self.definition(id) {
            spans.push(definition.span);
        }
        spans.extend(
            self.references
                .iter()
                .filter(|reference| reference.definition == id)
                .map(|reference| reference.span),
        );
        spans
    }

    fn define(&mut self, name: String, span: Span, kind: SymbolKind) -> DefinitionId {
        let id = DefinitionId(self.definitions.len() as u32);
        self.definitions.push(Definition {
            id,
            name,
            span,
            kind,
        });
        id
    }
}

fn resolve(
    name: &str,
    inline: &HashMap<String, DefinitionId>,
    scopes: &[HashMap<String, DefinitionId>],
) -> Option<DefinitionId> {
    inline.get(name).copied().or_else(|| {
        scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
    })
}

fn contains(span: Span, offset: usize) -> bool {
    span.start <= offset && offset < span.end
}

fn declaration_kind(token: &Token) -> Option<SymbolKind> {
    match token.kind {
        TokenKind::F => Some(SymbolKind::Function),
        TokenKind::S | TokenKind::E | TokenKind::T | TokenKind::Type => Some(SymbolKind::Type),
        TokenKind::Md => Some(SymbolKind::Module),
        // Canonical keywords are contextual in the scanner and may deliberately
        // remain identifiers until the parser sees their declaration position.
        TokenKind::Ident(_) => match token.lexeme.as_str() {
            "f" | "fn" | "function" => Some(SymbolKind::Function),
            "s" | "struct" | "e" | "enum" | "t" | "trait" | "type" => Some(SymbolKind::Type),
            "md" | "module" => Some(SymbolKind::Module),
            _ => None,
        },
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Scanner;

    fn index(source: &str) -> SemanticIndex {
        let (tokens, errors) = Scanner::new(source).scan_all();
        assert!(errors.is_empty());
        SemanticIndex::from_tokens(&tokens)
    }

    #[test]
    fn shadowed_locals_have_distinct_identities() {
        let source = "f main() -> Int\n    x = 1\n    if true\n        x = 2\n        print(x)\n    print(x)\n    x\n";
        let index = index(source);
        let xs: Vec<_> = index
            .definitions()
            .iter()
            .filter(|definition| definition.name == "x")
            .collect();
        assert_eq!(xs.len(), 2);
        assert_eq!(index.references_to(xs[0].id, false).len(), 2);
        assert_eq!(index.references_to(xs[1].id, false).len(), 1);
    }

    #[test]
    fn parameter_references_resolve_to_parameter() {
        let source = "f identity(value: Int) -> Int = value\n";
        let index = index(source);
        let parameter = index
            .definitions()
            .iter()
            .find(|definition| definition.kind == SymbolKind::Parameter)
            .unwrap_or_else(|| panic!("definitions: {:?}", index.definitions()));
        assert_eq!(index.references_to(parameter.id, false).len(), 1);
    }
}
