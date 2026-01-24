//! Recursive descent parser for FORMA.
//!
//! This module implements a hand-written recursive descent parser that
//! handles FORMA's indentation-significant syntax.

use crate::errors::{ParseError, Result};
use crate::lexer::{FStringPart, Span, Token, TokenKind};
use crate::parser::ast::*;

/// The parser for FORMA source code.
pub struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, pos: 0 }
    }

    /// Parse a complete source file.
    pub fn parse(mut self) -> Result<SourceFile> {
        let start = self.current_span();
        let mut items = Vec::new();

        while !self.at_end() {
            // Skip newlines at top level
            while self.check(TokenKind::Newline) {
                self.advance();
            }
            if self.at_end() {
                break;
            }
            items.push(self.parse_item()?);
        }

        let end = self.previous_span();
        Ok(SourceFile {
            items,
            span: start.merge(end),
        })
    }

    // ========================================================================
    // Items
    // ========================================================================

    fn parse_item(&mut self) -> Result<Item> {
        let attrs = self.parse_attributes()?;
        let start = self.current_span();

        // Check for visibility
        let vis = self.parse_visibility()?;

        // Check for async/unsafe modifiers
        let is_async = self.match_token(TokenKind::As);
        let is_unsafe = self.match_token(TokenKind::Un);

        let mut kind = if self.check(TokenKind::F) {
            self.parse_function(is_async, is_unsafe, vis)?
        } else if self.check(TokenKind::S) {
            ItemKind::Struct(self.parse_struct(vis)?)
        } else if self.check(TokenKind::E) {
            ItemKind::Enum(self.parse_enum(vis)?)
        } else if self.check(TokenKind::T) {
            ItemKind::Trait(self.parse_trait(is_unsafe, vis)?)
        } else if self.check(TokenKind::I) {
            ItemKind::Impl(self.parse_impl(is_unsafe)?)
        } else if self.check(TokenKind::Type) {
            ItemKind::TypeAlias(self.parse_type_alias()?)
        } else if self.check(TokenKind::Us) {
            ItemKind::Use(self.parse_use()?)
        } else if self.check(TokenKind::Md) {
            ItemKind::Module(self.parse_module(vis)?)
        } else if self.check_ident() && self.peek_is(TokenKind::ColonColon) {
            ItemKind::Const(self.parse_const(vis)?)
        } else {
            return Err(self.error("expected item (f, s, e, t, i, type, us, md)"));
        };

        // Extract @pre/@post contracts and add them to the function
        if let ItemKind::Function(ref mut func) = kind {
            for attr in &attrs {
                if attr.name.name == "pre" || attr.name.name == "post" {
                    if let Some(contract) = Self::extract_contract(attr) {
                        if attr.name.name == "pre" {
                            func.preconditions.push(contract);
                        } else {
                            func.postconditions.push(contract);
                        }
                    }
                }
            }
        }

        // Filter out @pre/@post from attrs (they're now in the function)
        let remaining_attrs: Vec<_> = attrs
            .into_iter()
            .filter(|a| a.name.name != "pre" && a.name.name != "post")
            .collect();

        let end = self.previous_span();
        Ok(Item {
            kind,
            attrs: remaining_attrs,
            span: start.merge(end),
        })
    }

    /// Extract a Contract from a @pre or @post attribute
    fn extract_contract(attr: &Attribute) -> Option<Contract> {
        // Find the condition expression in the args
        let condition_arg = attr.args.iter().find(|a| a.name.name == "condition")?;
        let condition = condition_arg.expr.clone()?;

        // Find optional message
        let message = attr.args.iter()
            .find(|a| a.name.name == "message")
            .and_then(|a| {
                if let Some(lit) = &a.value {
                    if let LiteralKind::String(s) = &lit.kind {
                        return Some(s.clone());
                    }
                }
                None
            });

        Some(Contract {
            condition,
            message,
            span: attr.span,
        })
    }

    fn parse_attributes(&mut self) -> Result<Vec<Attribute>> {
        let mut attrs = Vec::new();
        while self.check(TokenKind::At) {
            attrs.push(self.parse_attribute()?);
            self.skip_newlines();
        }
        Ok(attrs)
    }

    fn parse_attribute(&mut self) -> Result<Attribute> {
        let start = self.current_span();
        self.expect(TokenKind::At)?;
        let name = self.parse_ident()?;

        // Check for contract attributes that take expression arguments
        let is_contract = name.name == "pre" || name.name == "post";

        let args = if self.match_token(TokenKind::LParen) {
            if is_contract {
                self.parse_contract_attr_args()?
            } else {
                let args = self.parse_attr_args()?;
                self.expect(TokenKind::RParen)?;
                args
            }
        } else {
            Vec::new()
        };

        Ok(Attribute {
            name,
            args,
            span: start.merge(self.previous_span()),
        })
    }

    /// Parse contract attribute arguments: @pre(condition) or @pre(condition, "message")
    fn parse_contract_attr_args(&mut self) -> Result<Vec<AttrArg>> {
        let start = self.current_span();
        let expr = self.parse_expr()?;
        let mut args = vec![AttrArg {
            name: Ident::new("condition", start),
            value: None,
            expr: Some(Box::new(expr)),
            span: start.merge(self.previous_span()),
        }];

        // Optional message
        if self.match_token(TokenKind::Comma) {
            let msg_start = self.current_span();
            if let Some(TokenKind::String(s)) = self.current_kind() {
                let msg = s.clone();
                self.advance();
                args.push(AttrArg {
                    name: Ident::new("message", msg_start),
                    value: Some(Literal {
                        kind: LiteralKind::String(msg.clone()),
                        span: msg_start.merge(self.previous_span()),
                    }),
                    expr: None,
                    span: msg_start.merge(self.previous_span()),
                });
            }
        }

        self.expect(TokenKind::RParen)?;
        Ok(args)
    }

    fn parse_attr_args(&mut self) -> Result<Vec<AttrArg>> {
        let mut args = Vec::new();
        if !self.check(TokenKind::RParen) {
            loop {
                let start = self.current_span();
                let name = self.parse_ident()?;
                let value = if self.match_token(TokenKind::Eq) {
                    Some(self.parse_literal()?)
                } else {
                    None
                };
                args.push(AttrArg {
                    name,
                    value,
                    expr: None,
                    span: start.merge(self.previous_span()),
                });
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }
        Ok(args)
    }

    fn parse_visibility(&mut self) -> Result<Visibility> {
        if self.match_token(TokenKind::Pub) {
            // Check for pub(crate)
            if self.match_token(TokenKind::LParen) {
                if self.check_ident_str("crate") {
                    self.advance();
                    self.expect(TokenKind::RParen)?;
                    return Ok(Visibility::Crate);
                }
                self.expect(TokenKind::RParen)?;
            }
            Ok(Visibility::Public)
        } else {
            Ok(Visibility::Private)
        }
    }

    fn parse_function(&mut self, is_async: bool, is_unsafe: bool, vis: Visibility) -> Result<ItemKind> {
        let start = self.current_span();
        self.expect(TokenKind::F)?;
        let name = self.parse_ident()?;

        let generics = self.parse_optional_generics()?;
        let params = self.parse_fn_params()?;
        let return_type = self.parse_optional_return_type()?;

        let body = if self.match_token(TokenKind::Eq) {
            // Single expression body: `f foo -> Int = 42`
            let expr = self.parse_expr()?;
            Some(FnBody::Expr(Box::new(expr)))
        } else if self.check(TokenKind::LBrace) {
            // Brace block
            Some(FnBody::Block(self.parse_brace_block()?))
        } else if self.check(TokenKind::Newline) {
            // Indented block
            self.advance(); // consume newline
            if self.check(TokenKind::Indent) {
                Some(FnBody::Block(self.parse_indent_block()?))
            } else {
                // No body (trait method signature)
                None
            }
        } else {
            None
        };

        Ok(ItemKind::Function(Function {
            name,
            generics,
            params,
            return_type,
            body,
            is_async,
            is_unsafe,
            visibility: vis,
            preconditions: Vec::new(),
            postconditions: Vec::new(),
            span: start.merge(self.previous_span()),
        }))
    }

    fn parse_fn_params(&mut self) -> Result<Vec<Param>> {
        if !self.match_token(TokenKind::LParen) {
            return Ok(Vec::new());
        }

        let mut params = Vec::new();
        if !self.check(TokenKind::RParen) {
            loop {
                params.push(self.parse_param()?);
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }
        self.expect(TokenKind::RParen)?;
        Ok(params)
    }

    fn parse_param(&mut self) -> Result<Param> {
        let start = self.current_span();

        // Handle &self and &mut self
        if self.check(TokenKind::Amp) {
            self.advance();
            let is_mut = self.match_token(TokenKind::Mut);
            if self.check_ident_str("self") {
                let name = self.parse_ident()?;
                let ty = Type {
                    kind: TypeKind::Ref(
                        Box::new(Type {
                            kind: TypeKind::Path(TypePath {
                                segments: vec![TypePathSegment {
                                    name: Ident::new("Self", start),
                                    args: None,
                                    span: start,
                                }],
                                span: start,
                            }),
                            span: start,
                        }),
                        is_mut,
                    ),
                    span: start.merge(self.previous_span()),
                };
                return Ok(Param {
                    name,
                    ty,
                    default: None,
                    span: start.merge(self.previous_span()),
                });
            }
        }

        // Handle self
        if self.check_ident_str("self") {
            let name = self.parse_ident()?;
            let ty = Type {
                kind: TypeKind::Path(TypePath {
                    segments: vec![TypePathSegment {
                        name: Ident::new("Self", start),
                        args: None,
                        span: start,
                    }],
                    span: start,
                }),
                span: start,
            };
            return Ok(Param {
                name,
                ty,
                default: None,
                span: start.merge(self.previous_span()),
            });
        }

        let name = self.parse_ident()?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;
        let default = if self.match_token(TokenKind::Eq) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        Ok(Param {
            name,
            ty,
            default,
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_optional_return_type(&mut self) -> Result<Option<Type>> {
        if self.match_token(TokenKind::Arrow) {
            Ok(Some(self.parse_type()?))
        } else {
            Ok(None)
        }
    }

    fn parse_struct(&mut self, vis: Visibility) -> Result<Struct> {
        let start = self.current_span();
        self.expect(TokenKind::S)?;
        let name = self.parse_ident()?;
        let generics = self.parse_optional_generics()?;

        let kind = if self.match_token(TokenKind::LParen) {
            // Tuple struct: `s Point(Int, Int)`
            if self.check(TokenKind::RParen) {
                self.advance();
                StructKind::Unit
            } else {
                // Check if it's named fields or just types
                if self.check_ident() && self.peek_is(TokenKind::Colon) {
                    // Named tuple fields: `s Point(x: Int, y: Int)`
                    let fields = self.parse_struct_fields_inline()?;
                    self.expect(TokenKind::RParen)?;
                    StructKind::Named(fields)
                } else {
                    // Type-only tuple: `s Point(Int, Int)`
                    let mut types = Vec::new();
                    loop {
                        types.push(self.parse_type()?);
                        if !self.match_token(TokenKind::Comma) {
                            break;
                        }
                    }
                    self.expect(TokenKind::RParen)?;
                    StructKind::Tuple(types)
                }
            }
        } else if self.check(TokenKind::LBrace) {
            // Brace style: `s Point { x: Int, y: Int }`
            self.advance();
            let fields = self.parse_struct_fields_inline()?;
            self.expect(TokenKind::RBrace)?;
            StructKind::Named(fields)
        } else if self.check(TokenKind::Newline) {
            // Indented style
            self.advance();
            if self.check(TokenKind::Indent) {
                self.advance();
                let fields = self.parse_struct_fields_indented()?;
                StructKind::Named(fields)
            } else {
                StructKind::Unit
            }
        } else {
            StructKind::Unit
        };

        Ok(Struct {
            name,
            generics,
            kind,
            visibility: vis,
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_struct_fields_inline(&mut self) -> Result<Vec<Field>> {
        let mut fields = Vec::new();
        if !self.check(TokenKind::RParen) && !self.check(TokenKind::RBrace) {
            loop {
                fields.push(self.parse_field()?);
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
                // Allow trailing comma
                if self.check(TokenKind::RParen) || self.check(TokenKind::RBrace) {
                    break;
                }
            }
        }
        Ok(fields)
    }

    fn parse_struct_fields_indented(&mut self) -> Result<Vec<Field>> {
        let mut fields = Vec::new();
        while !self.check(TokenKind::Dedent) && !self.at_end() {
            self.skip_newlines();
            if self.check(TokenKind::Dedent) {
                break;
            }
            fields.push(self.parse_field()?);
            self.skip_newlines();
        }
        if self.check(TokenKind::Dedent) {
            self.advance();
        }
        Ok(fields)
    }

    fn parse_field(&mut self) -> Result<Field> {
        let start = self.current_span();
        let vis = self.parse_visibility()?;
        let name = self.parse_ident()?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;
        let default = if self.match_token(TokenKind::Eq) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        Ok(Field {
            name,
            ty,
            default,
            visibility: vis,
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_enum(&mut self, vis: Visibility) -> Result<Enum> {
        let start = self.current_span();
        self.expect(TokenKind::E)?;
        let name = self.parse_ident()?;
        let generics = self.parse_optional_generics()?;

        let variants = if self.match_token(TokenKind::Eq) {
            // Inline style: `e Bool = True | False`
            self.parse_enum_variants_inline()?
        } else if self.check(TokenKind::LBrace) {
            self.advance();
            let variants = self.parse_enum_variants_brace()?;
            self.expect(TokenKind::RBrace)?;
            variants
        } else if self.check(TokenKind::Newline) {
            self.advance();
            if self.check(TokenKind::Indent) {
                self.advance();
                self.parse_enum_variants_indented()?
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };

        Ok(Enum {
            name,
            generics,
            variants,
            visibility: vis,
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_enum_variants_inline(&mut self) -> Result<Vec<Variant>> {
        let mut variants = Vec::new();
        loop {
            variants.push(self.parse_variant()?);
            if !self.match_token(TokenKind::Pipe) {
                break;
            }
        }
        Ok(variants)
    }

    fn parse_enum_variants_brace(&mut self) -> Result<Vec<Variant>> {
        let mut variants = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.at_end() {
            variants.push(self.parse_variant()?);
            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }
        Ok(variants)
    }

    fn parse_enum_variants_indented(&mut self) -> Result<Vec<Variant>> {
        let mut variants = Vec::new();
        while !self.check(TokenKind::Dedent) && !self.at_end() {
            self.skip_newlines();
            if self.check(TokenKind::Dedent) {
                break;
            }
            variants.push(self.parse_variant()?);
            self.skip_newlines();
        }
        if self.check(TokenKind::Dedent) {
            self.advance();
        }
        Ok(variants)
    }

    fn parse_variant(&mut self) -> Result<Variant> {
        let start = self.current_span();
        let name = self.parse_ident()?;

        let kind = if self.match_token(TokenKind::LParen) {
            if self.check(TokenKind::RParen) {
                self.advance();
                VariantKind::Unit
            } else if self.check_ident() && self.peek_is(TokenKind::Colon) {
                // Named fields
                let fields = self.parse_struct_fields_inline()?;
                self.expect(TokenKind::RParen)?;
                VariantKind::Named(fields)
            } else {
                // Tuple variant
                let mut types = Vec::new();
                loop {
                    types.push(self.parse_type()?);
                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                }
                self.expect(TokenKind::RParen)?;
                VariantKind::Tuple(types)
            }
        } else {
            VariantKind::Unit
        };

        Ok(Variant {
            name,
            kind,
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_trait(&mut self, is_unsafe: bool, vis: Visibility) -> Result<Trait> {
        let start = self.current_span();
        self.expect(TokenKind::T)?;
        let name = self.parse_ident()?;
        let generics = self.parse_optional_generics()?;

        let supertraits = if self.match_token(TokenKind::Colon) {
            self.parse_type_bounds()?
        } else {
            Vec::new()
        };

        let items = if self.check(TokenKind::LBrace) {
            self.advance();
            let items = self.parse_trait_items_brace()?;
            self.expect(TokenKind::RBrace)?;
            items
        } else if self.check(TokenKind::Newline) {
            self.advance();
            if self.check(TokenKind::Indent) {
                self.advance();
                self.parse_trait_items_indented()?
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };

        Ok(Trait {
            name,
            generics,
            supertraits,
            items,
            visibility: vis,
            is_unsafe,
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_trait_items_brace(&mut self) -> Result<Vec<TraitItem>> {
        let mut items = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.at_end() {
            self.skip_newlines();
            if self.check(TokenKind::RBrace) {
                break;
            }
            items.push(self.parse_trait_item()?);
            self.skip_newlines();
        }
        Ok(items)
    }

    fn parse_trait_items_indented(&mut self) -> Result<Vec<TraitItem>> {
        let mut items = Vec::new();
        while !self.check(TokenKind::Dedent) && !self.at_end() {
            self.skip_newlines();
            if self.check(TokenKind::Dedent) {
                break;
            }
            items.push(self.parse_trait_item()?);
            self.skip_newlines();
        }
        if self.check(TokenKind::Dedent) {
            self.advance();
        }
        Ok(items)
    }

    fn parse_trait_item(&mut self) -> Result<TraitItem> {
        if self.check(TokenKind::Type) {
            Ok(TraitItem::TypeAlias(self.parse_type_alias()?))
        } else if self.check(TokenKind::F) || self.check(TokenKind::As) {
            let is_async = self.match_token(TokenKind::As);
            match self.parse_function(is_async, false, Visibility::Private)? {
                ItemKind::Function(f) => Ok(TraitItem::Function(f)),
                _ => unreachable!(),
            }
        } else {
            Err(self.error("expected trait item (type or f)"))
        }
    }

    fn parse_impl(&mut self, is_unsafe: bool) -> Result<Impl> {
        let start = self.current_span();
        self.expect(TokenKind::I)?;

        let generics = self.parse_optional_generics()?;

        // Parse either:
        // - `i Type` (inherent impl)
        // - `i Trait for Type` (trait impl)
        let first_type = self.parse_type()?;

        let (trait_, self_type) = if self.match_token(TokenKind::For) {
            let self_type = self.parse_type()?;
            (Some(first_type), self_type)
        } else {
            (None, first_type)
        };

        let where_clause = self.parse_optional_where_clause()?;

        let items = if self.check(TokenKind::LBrace) {
            self.advance();
            let items = self.parse_impl_items_brace()?;
            self.expect(TokenKind::RBrace)?;
            items
        } else if self.check(TokenKind::Newline) {
            self.advance();
            if self.check(TokenKind::Indent) {
                self.advance();
                self.parse_impl_items_indented()?
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };

        Ok(Impl {
            generics,
            trait_,
            self_type,
            where_clause,
            items,
            is_unsafe,
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_impl_items_brace(&mut self) -> Result<Vec<ImplItem>> {
        let mut items = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.at_end() {
            self.skip_newlines();
            if self.check(TokenKind::RBrace) {
                break;
            }
            items.push(self.parse_impl_item()?);
            self.skip_newlines();
        }
        Ok(items)
    }

    fn parse_impl_items_indented(&mut self) -> Result<Vec<ImplItem>> {
        let mut items = Vec::new();
        while !self.check(TokenKind::Dedent) && !self.at_end() {
            self.skip_newlines();
            if self.check(TokenKind::Dedent) {
                break;
            }
            items.push(self.parse_impl_item()?);
            self.skip_newlines();
        }
        if self.check(TokenKind::Dedent) {
            self.advance();
        }
        Ok(items)
    }

    fn parse_impl_item(&mut self) -> Result<ImplItem> {
        if self.check(TokenKind::Type) {
            Ok(ImplItem::TypeAlias(self.parse_type_alias()?))
        } else if self.check(TokenKind::F) || self.check(TokenKind::As) || self.check(TokenKind::Pub) {
            let vis = self.parse_visibility()?;
            let is_async = self.match_token(TokenKind::As);
            match self.parse_function(is_async, false, vis)? {
                ItemKind::Function(f) => Ok(ImplItem::Function(f)),
                _ => unreachable!(),
            }
        } else {
            Err(self.error("expected impl item (type or f)"))
        }
    }

    fn parse_type_alias(&mut self) -> Result<TypeAlias> {
        let start = self.current_span();
        self.expect(TokenKind::Type)?;
        let name = self.parse_ident()?;
        let generics = self.parse_optional_generics()?;
        let ty = if self.match_token(TokenKind::Eq) {
            Some(self.parse_type()?)
        } else {
            None
        };

        Ok(TypeAlias {
            name,
            generics,
            ty,
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_use(&mut self) -> Result<Use> {
        let start = self.current_span();
        self.expect(TokenKind::Us)?;
        let tree = self.parse_use_tree()?;
        Ok(Use {
            tree,
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_use_tree(&mut self) -> Result<UseTree> {
        if self.match_token(TokenKind::Star) {
            return Ok(UseTree::Glob);
        }

        if self.match_token(TokenKind::LBrace) {
            let mut trees = Vec::new();
            loop {
                trees.push(self.parse_use_tree()?);
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
            self.expect(TokenKind::RBrace)?;
            return Ok(UseTree::Group(trees));
        }

        // Parse path segments
        let mut segments = vec![self.parse_ident()?];
        while self.match_token(TokenKind::Dot) {
            if self.check(TokenKind::Star) {
                self.advance();
                return Ok(UseTree::Path(segments, Some(Box::new(UseTree::Glob))));
            }
            if self.check(TokenKind::LBrace) {
                self.advance();
                let mut trees = Vec::new();
                loop {
                    trees.push(self.parse_use_tree()?);
                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                }
                self.expect(TokenKind::RBrace)?;
                return Ok(UseTree::Path(segments, Some(Box::new(UseTree::Group(trees)))));
            }
            segments.push(self.parse_ident()?);
        }

        // Check for rename
        if self.match_token(TokenKind::Arrow) {
            let alias = self.parse_ident()?;
            return Ok(UseTree::Rename(segments, alias));
        }

        Ok(UseTree::Path(segments, None))
    }

    fn parse_module(&mut self, vis: Visibility) -> Result<Module> {
        let start = self.current_span();
        self.expect(TokenKind::Md)?;
        let name = self.parse_ident()?;

        let items = if self.check(TokenKind::LBrace) {
            self.advance();
            let mut items = Vec::new();
            while !self.check(TokenKind::RBrace) && !self.at_end() {
                self.skip_newlines();
                if self.check(TokenKind::RBrace) {
                    break;
                }
                items.push(self.parse_item()?);
                self.skip_newlines();
            }
            self.expect(TokenKind::RBrace)?;
            Some(items)
        } else if self.check(TokenKind::Newline) {
            self.advance();
            if self.check(TokenKind::Indent) {
                self.advance();
                let mut items = Vec::new();
                while !self.check(TokenKind::Dedent) && !self.at_end() {
                    self.skip_newlines();
                    if self.check(TokenKind::Dedent) {
                        break;
                    }
                    items.push(self.parse_item()?);
                    self.skip_newlines();
                }
                if self.check(TokenKind::Dedent) {
                    self.advance();
                }
                Some(items)
            } else {
                None
            }
        } else {
            None
        };

        Ok(Module {
            name,
            items,
            visibility: vis,
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_const(&mut self, vis: Visibility) -> Result<Const> {
        let start = self.current_span();
        let name = self.parse_ident()?;
        self.expect(TokenKind::ColonColon)?;

        let ty = if self.check(TokenKind::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        let value = self.parse_expr()?;

        Ok(Const {
            name,
            ty,
            value,
            visibility: vis,
            span: start.merge(self.previous_span()),
        })
    }

    // ========================================================================
    // Generics
    // ========================================================================

    fn parse_optional_generics(&mut self) -> Result<Option<Generics>> {
        if !self.check(TokenKind::LBracket) {
            return Ok(None);
        }

        let start = self.current_span();
        self.advance();
        let mut params = Vec::new();

        if !self.check(TokenKind::RBracket) {
            loop {
                params.push(self.parse_generic_param()?);
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.expect(TokenKind::RBracket)?;

        Ok(Some(Generics {
            params,
            span: start.merge(self.previous_span()),
        }))
    }

    fn parse_generic_param(&mut self) -> Result<GenericParam> {
        let start = self.current_span();
        let name = self.parse_ident()?;

        // Check if it's a const param: `N: Int`
        if self.match_token(TokenKind::Colon) {
            // Could be bounds or const param - check if it looks like a type
            let ty = self.parse_type()?;
            // If we see '+' next, it's bounds, otherwise const param
            if self.check(TokenKind::Plus) {
                let mut bounds = vec![TypeBound {
                    path: match ty.kind {
                        TypeKind::Path(p) => p,
                        _ => return Err(self.error("expected type path in bounds")),
                    },
                    span: ty.span,
                }];
                while self.match_token(TokenKind::Plus) {
                    bounds.push(self.parse_type_bound()?);
                }
                Ok(GenericParam::Type(TypeParam {
                    name,
                    bounds,
                    span: start.merge(self.previous_span()),
                }))
            } else {
                Ok(GenericParam::Const(ConstParam {
                    name,
                    ty,
                    span: start.merge(self.previous_span()),
                }))
            }
        } else {
            Ok(GenericParam::Type(TypeParam {
                name,
                bounds: Vec::new(),
                span: start.merge(self.previous_span()),
            }))
        }
    }

    fn parse_type_bounds(&mut self) -> Result<Vec<TypeBound>> {
        let mut bounds = vec![self.parse_type_bound()?];
        while self.match_token(TokenKind::Plus) {
            bounds.push(self.parse_type_bound()?);
        }
        Ok(bounds)
    }

    fn parse_type_bound(&mut self) -> Result<TypeBound> {
        let start = self.current_span();
        let path = self.parse_type_path()?;
        Ok(TypeBound {
            path,
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_optional_where_clause(&mut self) -> Result<Option<WhereClause>> {
        if !self.check(TokenKind::Where) {
            return Ok(None);
        }

        let start = self.current_span();
        self.advance();

        let mut predicates = Vec::new();
        loop {
            self.skip_newlines();
            predicates.push(self.parse_where_predicate()?);
            if !self.check(TokenKind::Comma) && !self.check(TokenKind::Newline) {
                break;
            }
            if self.check(TokenKind::Comma) {
                self.advance();
            }
        }

        Ok(Some(WhereClause {
            predicates,
            span: start.merge(self.previous_span()),
        }))
    }

    fn parse_where_predicate(&mut self) -> Result<WherePredicate> {
        let start = self.current_span();
        let ty = self.parse_type()?;
        self.expect(TokenKind::Colon)?;
        let bounds = self.parse_type_bounds()?;

        Ok(WherePredicate {
            ty,
            bounds,
            span: start.merge(self.previous_span()),
        })
    }

    // ========================================================================
    // Types
    // ========================================================================

    fn parse_type(&mut self) -> Result<Type> {
        let start = self.current_span();

        let mut ty = self.parse_type_primary()?;

        // Check for type suffixes: ?, !, ->
        loop {
            if self.match_token(TokenKind::Question) {
                ty = Type {
                    kind: TypeKind::Option(Box::new(ty)),
                    span: start.merge(self.previous_span()),
                };
            } else if self.match_token(TokenKind::Bang) {
                let error_ty = if self.check_type_start() {
                    Some(Box::new(self.parse_type()?))
                } else {
                    None
                };
                ty = Type {
                    kind: TypeKind::Result(Box::new(ty), error_ty),
                    span: start.merge(self.previous_span()),
                };
            } else if self.match_token(TokenKind::Arrow) {
                // Function type
                let ret = self.parse_type()?;
                ty = Type {
                    kind: TypeKind::Fn(vec![ty], Box::new(ret)),
                    span: start.merge(self.previous_span()),
                };
            } else {
                break;
            }
        }

        Ok(ty)
    }

    fn parse_type_primary(&mut self) -> Result<Type> {
        let start = self.current_span();

        // Reference type: &T or &mut T
        if self.match_token(TokenKind::Amp) {
            let is_mut = self.match_token(TokenKind::Mut);
            let inner = self.parse_type()?;
            return Ok(Type {
                kind: TypeKind::Ref(Box::new(inner), is_mut),
                span: start.merge(self.previous_span()),
            });
        }

        // Pointer type: *T or *mut T
        if self.match_token(TokenKind::Star) {
            let is_mut = self.match_token(TokenKind::Mut);
            let inner = self.parse_type()?;
            return Ok(Type {
                kind: TypeKind::Ptr(Box::new(inner), is_mut),
                span: start.merge(self.previous_span()),
            });
        }

        // Tuple or function type: (A, B) or (A, B) -> C
        if self.match_token(TokenKind::LParen) {
            let mut types = Vec::new();
            if !self.check(TokenKind::RParen) {
                loop {
                    types.push(self.parse_type()?);
                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                }
            }
            self.expect(TokenKind::RParen)?;

            // Check for function type
            if self.match_token(TokenKind::Arrow) {
                let ret = self.parse_type()?;
                return Ok(Type {
                    kind: TypeKind::Fn(types, Box::new(ret)),
                    span: start.merge(self.previous_span()),
                });
            }

            return Ok(Type {
                kind: TypeKind::Tuple(types),
                span: start.merge(self.previous_span()),
            });
        }

        // List, Array, or generic: [T], [T; N], or Type[Args]
        if self.match_token(TokenKind::LBracket) {
            let inner = self.parse_type()?;

            if self.match_token(TokenKind::Semicolon) {
                // Array: [T; N]
                let size = self.parse_expr()?;
                self.expect(TokenKind::RBracket)?;
                return Ok(Type {
                    kind: TypeKind::Array(Box::new(inner), Box::new(size)),
                    span: start.merge(self.previous_span()),
                });
            }

            self.expect(TokenKind::RBracket)?;
            return Ok(Type {
                kind: TypeKind::List(Box::new(inner)),
                span: start.merge(self.previous_span()),
            });
        }

        // Map or Set: {K: V} or {T}
        if self.match_token(TokenKind::LBrace) {
            let first = self.parse_type()?;

            if self.match_token(TokenKind::Colon) {
                // Map: {K: V}
                let value = self.parse_type()?;
                self.expect(TokenKind::RBrace)?;
                return Ok(Type {
                    kind: TypeKind::Map(Box::new(first), Box::new(value)),
                    span: start.merge(self.previous_span()),
                });
            }

            // Set: {T}
            self.expect(TokenKind::RBrace)?;
            return Ok(Type {
                kind: TypeKind::Set(Box::new(first)),
                span: start.merge(self.previous_span()),
            });
        }

        // Infer type: _
        if self.check_ident_str("_") {
            self.advance();
            return Ok(Type {
                kind: TypeKind::Infer,
                span: start.merge(self.previous_span()),
            });
        }

        // Never type: !
        if self.match_token(TokenKind::Bang) {
            return Ok(Type {
                kind: TypeKind::Never,
                span: start.merge(self.previous_span()),
            });
        }

        // Named type path
        let path = self.parse_type_path()?;
        Ok(Type {
            kind: TypeKind::Path(path),
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_type_path(&mut self) -> Result<TypePath> {
        let start = self.current_span();
        let mut segments = vec![self.parse_type_path_segment()?];

        while self.match_token(TokenKind::Dot) {
            segments.push(self.parse_type_path_segment()?);
        }

        Ok(TypePath {
            segments,
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_type_path_segment(&mut self) -> Result<TypePathSegment> {
        let start = self.current_span();
        let name = self.parse_ident()?;

        let args = if self.check(TokenKind::LBracket) {
            Some(self.parse_generic_args()?)
        } else {
            None
        };

        Ok(TypePathSegment {
            name,
            args,
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_generic_args(&mut self) -> Result<GenericArgs> {
        let start = self.current_span();
        self.expect(TokenKind::LBracket)?;

        let mut args = Vec::new();
        if !self.check(TokenKind::RBracket) {
            loop {
                // Try to parse as type first
                args.push(GenericArg::Type(self.parse_type()?));
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.expect(TokenKind::RBracket)?;

        Ok(GenericArgs {
            args,
            span: start.merge(self.previous_span()),
        })
    }

    fn check_type_start(&mut self) -> bool {
        self.check_ident()
            || self.check(TokenKind::LParen)
            || self.check(TokenKind::LBracket)
            || self.check(TokenKind::LBrace)
            || self.check(TokenKind::Amp)
            || self.check(TokenKind::Star)
            || self.check(TokenKind::Bang)
    }

    // ========================================================================
    // Expressions
    // ========================================================================

    pub fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr> {
        let start = self.current_span();
        let expr = self.parse_pipeline()?;

        if self.match_token(TokenKind::Eq) {
            let value = self.parse_assignment()?;
            return Ok(Expr {
                kind: ExprKind::Assign(Box::new(expr), Box::new(value), false),
                span: start.merge(self.previous_span()),
            });
        }

        if self.match_token(TokenKind::ColonEq) {
            let value = self.parse_assignment()?;
            return Ok(Expr {
                kind: ExprKind::Assign(Box::new(expr), Box::new(value), true),
                span: start.merge(self.previous_span()),
            });
        }

        // Compound assignment
        let op = if self.match_token(TokenKind::PlusEq) {
            Some(BinOp::Add)
        } else if self.match_token(TokenKind::MinusEq) {
            Some(BinOp::Sub)
        } else if self.match_token(TokenKind::StarEq) {
            Some(BinOp::Mul)
        } else if self.match_token(TokenKind::SlashEq) {
            Some(BinOp::Div)
        } else {
            None
        };

        if let Some(op) = op {
            let value = self.parse_assignment()?;
            return Ok(Expr {
                kind: ExprKind::AssignOp(Box::new(expr), op, Box::new(value)),
                span: start.merge(self.previous_span()),
            });
        }

        Ok(expr)
    }

    fn parse_pipeline(&mut self) -> Result<Expr> {
        let start = self.current_span();
        let mut expr = self.parse_coalesce()?;

        while self.match_token(TokenKind::Pipe) {
            // Don't confuse with || (already consumed as single |)
            let right = self.parse_coalesce()?;
            expr = Expr {
                kind: ExprKind::Pipeline(Box::new(expr), Box::new(right)),
                span: start.merge(self.previous_span()),
            };
        }

        Ok(expr)
    }

    fn parse_coalesce(&mut self) -> Result<Expr> {
        let start = self.current_span();
        let mut expr = self.parse_or()?;

        while self.match_token(TokenKind::QuestionQuestion) {
            let right = self.parse_or()?;
            expr = Expr {
                kind: ExprKind::Coalesce(Box::new(expr), Box::new(right)),
                span: start.merge(self.previous_span()),
            };
        }

        Ok(expr)
    }

    fn parse_or(&mut self) -> Result<Expr> {
        let start = self.current_span();
        let mut expr = self.parse_and()?;

        while self.match_token(TokenKind::PipePipe) {
            let right = self.parse_and()?;
            expr = Expr {
                kind: ExprKind::Binary(Box::new(expr), BinOp::Or, Box::new(right)),
                span: start.merge(self.previous_span()),
            };
        }

        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<Expr> {
        let start = self.current_span();
        let mut expr = self.parse_comparison()?;

        while self.match_token(TokenKind::AmpAmp) {
            let right = self.parse_comparison()?;
            expr = Expr {
                kind: ExprKind::Binary(Box::new(expr), BinOp::And, Box::new(right)),
                span: start.merge(self.previous_span()),
            };
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr> {
        let start = self.current_span();
        let mut expr = self.parse_bitor()?;

        let op = if self.match_token(TokenKind::EqEq) {
            Some(BinOp::Eq)
        } else if self.match_token(TokenKind::BangEq) {
            Some(BinOp::Ne)
        } else if self.match_token(TokenKind::Lt) {
            Some(BinOp::Lt)
        } else if self.match_token(TokenKind::LtEq) {
            Some(BinOp::Le)
        } else if self.match_token(TokenKind::Gt) {
            Some(BinOp::Gt)
        } else if self.match_token(TokenKind::GtEq) {
            Some(BinOp::Ge)
        } else {
            None
        };

        if let Some(op) = op {
            let right = self.parse_bitor()?;
            expr = Expr {
                kind: ExprKind::Binary(Box::new(expr), op, Box::new(right)),
                span: start.merge(self.previous_span()),
            };
        }

        Ok(expr)
    }

    fn parse_bitor(&mut self) -> Result<Expr> {
        // Note: single | is used for pipeline, not bitor in expressions
        // Bitor would need special handling if needed
        self.parse_bitxor()
    }

    fn parse_bitxor(&mut self) -> Result<Expr> {
        let start = self.current_span();
        let mut expr = self.parse_bitand()?;

        while self.match_token(TokenKind::Caret) {
            let right = self.parse_bitand()?;
            expr = Expr {
                kind: ExprKind::Binary(Box::new(expr), BinOp::BitXor, Box::new(right)),
                span: start.merge(self.previous_span()),
            };
        }

        Ok(expr)
    }

    fn parse_bitand(&mut self) -> Result<Expr> {
        // Note: single & is used for references, not bitand in most contexts
        self.parse_shift()
    }

    fn parse_shift(&mut self) -> Result<Expr> {
        let start = self.current_span();
        let mut expr = self.parse_additive()?;

        loop {
            let op = if self.match_token(TokenKind::LtLt) {
                BinOp::Shl
            } else if self.match_token(TokenKind::GtGt) {
                BinOp::Shr
            } else {
                break;
            };

            let right = self.parse_additive()?;
            expr = Expr {
                kind: ExprKind::Binary(Box::new(expr), op, Box::new(right)),
                span: start.merge(self.previous_span()),
            };
        }

        Ok(expr)
    }

    fn parse_additive(&mut self) -> Result<Expr> {
        let start = self.current_span();
        let mut expr = self.parse_multiplicative()?;

        loop {
            let op = if self.match_token(TokenKind::Plus) {
                BinOp::Add
            } else if self.match_token(TokenKind::Minus) {
                BinOp::Sub
            } else {
                break;
            };

            let right = self.parse_multiplicative()?;
            expr = Expr {
                kind: ExprKind::Binary(Box::new(expr), op, Box::new(right)),
                span: start.merge(self.previous_span()),
            };
        }

        Ok(expr)
    }

    fn parse_multiplicative(&mut self) -> Result<Expr> {
        let start = self.current_span();
        let mut expr = self.parse_unary()?;

        loop {
            let op = if self.match_token(TokenKind::Star) {
                BinOp::Mul
            } else if self.match_token(TokenKind::Slash) {
                BinOp::Div
            } else if self.match_token(TokenKind::Percent) {
                BinOp::Mod
            } else {
                break;
            };

            let right = self.parse_unary()?;
            expr = Expr {
                kind: ExprKind::Binary(Box::new(expr), op, Box::new(right)),
                span: start.merge(self.previous_span()),
            };
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr> {
        let start = self.current_span();

        if self.match_token(TokenKind::Minus) {
            let expr = self.parse_unary()?;
            return Ok(Expr {
                kind: ExprKind::Unary(UnaryOp::Neg, Box::new(expr)),
                span: start.merge(self.previous_span()),
            });
        }

        if self.match_token(TokenKind::Bang) {
            let expr = self.parse_unary()?;
            return Ok(Expr {
                kind: ExprKind::Unary(UnaryOp::Not, Box::new(expr)),
                span: start.merge(self.previous_span()),
            });
        }

        if self.match_token(TokenKind::Amp) {
            let is_mut = self.match_token(TokenKind::Mut);
            let expr = self.parse_unary()?;
            let op = if is_mut { UnaryOp::RefMut } else { UnaryOp::Ref };
            return Ok(Expr {
                kind: ExprKind::Unary(op, Box::new(expr)),
                span: start.merge(self.previous_span()),
            });
        }

        if self.match_token(TokenKind::Star) {
            let expr = self.parse_unary()?;
            return Ok(Expr {
                kind: ExprKind::Unary(UnaryOp::Deref, Box::new(expr)),
                span: start.merge(self.previous_span()),
            });
        }

        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expr> {
        let start = self.current_span();
        let mut expr = self.parse_primary()?;

        loop {
            if self.match_token(TokenKind::Dot) {
                // Field access or method call
                if let Some(TokenKind::Int(n)) = self.current_kind() {
                    // Tuple field access: expr.0
                    self.advance();
                    expr = Expr {
                        kind: ExprKind::TupleField(Box::new(expr), n as usize),
                        span: start.merge(self.previous_span()),
                    };
                } else {
                    let name = self.parse_ident()?;
                    if self.check(TokenKind::LParen) {
                        // Method call
                        let args = self.parse_call_args()?;
                        expr = Expr {
                            kind: ExprKind::MethodCall(Box::new(expr), name, args),
                            span: start.merge(self.previous_span()),
                        };
                    } else {
                        // Field access
                        expr = Expr {
                            kind: ExprKind::Field(Box::new(expr), name),
                            span: start.merge(self.previous_span()),
                        };
                    }
                }
            } else if self.check(TokenKind::LParen) {
                // Function call
                let args = self.parse_call_args()?;
                expr = Expr {
                    kind: ExprKind::Call(Box::new(expr), args),
                    span: start.merge(self.previous_span()),
                };
            } else if self.match_token(TokenKind::LBracket) {
                // Index
                let index = self.parse_expr()?;
                self.expect(TokenKind::RBracket)?;
                expr = Expr {
                    kind: ExprKind::Index(Box::new(expr), Box::new(index)),
                    span: start.merge(self.previous_span()),
                };
            } else if self.match_token(TokenKind::Question) {
                // Error propagation
                expr = Expr {
                    kind: ExprKind::Try(Box::new(expr)),
                    span: start.merge(self.previous_span()),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_call_args(&mut self) -> Result<Vec<Arg>> {
        self.expect(TokenKind::LParen)?;
        let mut args = Vec::new();

        if !self.check(TokenKind::RParen) {
            loop {
                let start = self.current_span();

                // Check for named argument
                let (name, value) = if self.check_ident() && self.peek_is(TokenKind::Colon) {
                    let name = self.parse_ident()?;
                    self.expect(TokenKind::Colon)?;
                    let value = self.parse_expr()?;
                    (Some(name), value)
                } else {
                    (None, self.parse_expr()?)
                };

                args.push(Arg {
                    name,
                    value,
                    span: start.merge(self.previous_span()),
                });

                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.expect(TokenKind::RParen)?;
        Ok(args)
    }

    fn parse_primary(&mut self) -> Result<Expr> {
        let start = self.current_span();

        // F-strings (interpolated strings): f"Hello {name}!"
        if let Some(TokenKind::FString(parts)) = self.current_kind() {
            self.advance();
            return self.parse_fstring(parts, start);
        }

        // Literals
        if let Some(lit) = self.try_parse_literal()? {
            // Check for range: literal..end or literal..=end
            if self.check(TokenKind::DotDot) || self.check(TokenKind::DotDotEq) {
                let inclusive = self.match_token(TokenKind::DotDotEq);
                if !inclusive {
                    self.expect(TokenKind::DotDot)?;
                }
                let start_expr = Expr {
                    kind: ExprKind::Literal(lit),
                    span: start,
                };
                let end = if self.check_expr_start() {
                    Some(Box::new(self.parse_unary()?))
                } else {
                    None
                };
                return Ok(Expr {
                    kind: ExprKind::Range(Some(Box::new(start_expr)), end, inclusive),
                    span: start.merge(self.previous_span()),
                });
            }
            return Ok(Expr {
                kind: ExprKind::Literal(lit),
                span: start.merge(self.previous_span()),
            });
        }

        // Keywords
        if self.match_token(TokenKind::If) {
            return self.parse_if_expr(start);
        }

        if self.check(TokenKind::M) {
            return self.parse_match_expr();
        }

        if self.match_token(TokenKind::For) {
            return self.parse_for_expr(start);
        }

        if self.check(TokenKind::Wh) {
            return self.parse_while_expr();
        }

        if self.check(TokenKind::Lp) {
            return self.parse_loop_expr();
        }

        if self.match_token(TokenKind::Ret) {
            let value = if self.check_expr_start() {
                Some(Box::new(self.parse_expr()?))
            } else {
                None
            };
            return Ok(Expr {
                kind: ExprKind::Return(value),
                span: start.merge(self.previous_span()),
            });
        }

        if self.match_token(TokenKind::Br) {
            let label = None; // TODO: parse labels
            let value = if self.check_expr_start() {
                Some(Box::new(self.parse_expr()?))
            } else {
                None
            };
            return Ok(Expr {
                kind: ExprKind::Break(label, value),
                span: start.merge(self.previous_span()),
            });
        }

        if self.match_token(TokenKind::Ct) {
            return Ok(Expr {
                kind: ExprKind::Continue(None),
                span: start.merge(self.previous_span()),
            });
        }

        if self.match_token(TokenKind::Aw) {
            let expr = self.parse_unary()?;
            return Ok(Expr {
                kind: ExprKind::Await(Box::new(expr)),
                span: start.merge(self.previous_span()),
            });
        }

        if self.match_token(TokenKind::As) {
            // Async block
            let block = self.parse_block()?;
            return Ok(Expr {
                kind: ExprKind::Async(block),
                span: start.merge(self.previous_span()),
            });
        }

        if self.match_token(TokenKind::Un) {
            // Unsafe block
            let block = self.parse_block()?;
            return Ok(Expr {
                kind: ExprKind::Unsafe(block),
                span: start.merge(self.previous_span()),
            });
        }

        // Closure: |params| body
        if self.check(TokenKind::Pipe) || self.check(TokenKind::PipePipe) {
            return self.parse_closure();
        }

        // Field shorthand: .name
        if self.match_token(TokenKind::Dot) {
            if self.check_ident() {
                let name = self.parse_ident()?;
                return Ok(Expr {
                    kind: ExprKind::FieldShorthand(name),
                    span: start.merge(self.previous_span()),
                });
            }
            return Err(self.error("expected identifier after '.'"));
        }

        // Parenthesized expression, tuple, or operator shorthand
        if self.match_token(TokenKind::LParen) {
            // Check for operator shorthand: (+ 10), (* 2)
            if let Some(op) = self.try_parse_op_shorthand()? {
                return Ok(op);
            }

            // Empty tuple
            if self.match_token(TokenKind::RParen) {
                return Ok(Expr {
                    kind: ExprKind::Tuple(Vec::new()),
                    span: start.merge(self.previous_span()),
                });
            }

            let first = self.parse_expr()?;

            if self.match_token(TokenKind::Comma) {
                // Tuple
                let mut elements = vec![first];
                if !self.check(TokenKind::RParen) {
                    loop {
                        elements.push(self.parse_expr()?);
                        if !self.match_token(TokenKind::Comma) {
                            break;
                        }
                        if self.check(TokenKind::RParen) {
                            break;
                        }
                    }
                }
                self.expect(TokenKind::RParen)?;
                return Ok(Expr {
                    kind: ExprKind::Tuple(elements),
                    span: start.merge(self.previous_span()),
                });
            }

            self.expect(TokenKind::RParen)?;
            return Ok(Expr {
                kind: ExprKind::Paren(Box::new(first)),
                span: start.merge(self.previous_span()),
            });
        }

        // Array/List literal
        if self.match_token(TokenKind::LBracket) {
            if self.match_token(TokenKind::RBracket) {
                return Ok(Expr {
                    kind: ExprKind::Array(Vec::new()),
                    span: start.merge(self.previous_span()),
                });
            }

            let first = self.parse_expr()?;

            // Array repeat: [value; count]
            if self.match_token(TokenKind::Semicolon) {
                let count = self.parse_expr()?;
                self.expect(TokenKind::RBracket)?;
                return Ok(Expr {
                    kind: ExprKind::ArrayRepeat(Box::new(first), Box::new(count)),
                    span: start.merge(self.previous_span()),
                });
            }

            // Array literal
            let mut elements = vec![first];
            while self.match_token(TokenKind::Comma) {
                if self.check(TokenKind::RBracket) {
                    break;
                }
                elements.push(self.parse_expr()?);
            }
            self.expect(TokenKind::RBracket)?;
            return Ok(Expr {
                kind: ExprKind::Array(elements),
                span: start.merge(self.previous_span()),
            });
        }

        // Map or Set literal
        if self.match_token(TokenKind::LBrace) {
            return self.parse_map_or_set(start);
        }

        // Range with no start: ..end or ..=end
        if self.check(TokenKind::DotDot) || self.check(TokenKind::DotDotEq) {
            let inclusive = self.match_token(TokenKind::DotDotEq);
            if !inclusive {
                self.expect(TokenKind::DotDot)?;
            }
            let end = if self.check_expr_start() {
                Some(Box::new(self.parse_unary()?))
            } else {
                None
            };
            return Ok(Expr {
                kind: ExprKind::Range(None, end, inclusive),
                span: start.merge(self.previous_span()),
            });
        }

        // Identifier or path
        if self.check_ident() {
            let name = self.parse_ident()?;

            // Check for struct literal: Name { ... } or Name(...)
            // Look ahead past newlines to allow multi-line struct literals like:
            //   token = Token {
            //       field: value
            //   }
            // But preserve newlines if not a struct literal (they may be significant)
            let saved_pos = self.pos;
            self.skip_newlines();
            if self.check(TokenKind::LBrace) {
                return self.parse_struct_expr(name, start);
            }
            // Restore position - newlines are significant for indentation
            self.pos = saved_pos;

            // Check for range: name..end or name..=end
            if self.check(TokenKind::DotDot) || self.check(TokenKind::DotDotEq) {
                let inclusive = self.match_token(TokenKind::DotDotEq);
                if !inclusive {
                    self.expect(TokenKind::DotDot)?;
                }
                let start_expr = Expr {
                    kind: ExprKind::Ident(name),
                    span: start,
                };
                let end = if self.check_expr_start() {
                    Some(Box::new(self.parse_unary()?))
                } else {
                    None
                };
                return Ok(Expr {
                    kind: ExprKind::Range(Some(Box::new(start_expr)), end, inclusive),
                    span: start.merge(self.previous_span()),
                });
            }

            return Ok(Expr {
                kind: ExprKind::Ident(name),
                span: start.merge(self.previous_span()),
            });
        }

        Err(self.error("expected expression"))
    }

    /// Parse an f-string into a concatenation of string literals and str() calls.
    /// f"Hello {name}!" becomes "Hello " + str(name) + "!"
    fn parse_fstring(&mut self, parts: Vec<FStringPart>, span: Span) -> Result<Expr> {
        if parts.is_empty() {
            // Empty f-string: f"" -> ""
            return Ok(Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::String(String::new()),
                    span,
                }),
                span,
            });
        }

        // Convert parts to expressions
        let mut exprs: Vec<Expr> = Vec::new();
        for part in parts {
            match part {
                FStringPart::Text(s) => {
                    if !s.is_empty() {
                        exprs.push(Expr {
                            kind: ExprKind::Literal(Literal {
                                kind: LiteralKind::String(s),
                                span,
                            }),
                            span,
                        });
                    }
                }
                FStringPart::Expr(expr_src) => {
                    // Parse the expression from source
                    let expr = self.parse_embedded_expr(&expr_src, span)?;
                    // Wrap in str() call
                    let str_call = Expr {
                        kind: ExprKind::Call(
                            Box::new(Expr {
                                kind: ExprKind::Ident(Ident::new("str", span)),
                                span,
                            }),
                            vec![Arg { name: None, value: expr, span }],
                        ),
                        span,
                    };
                    exprs.push(str_call);
                }
            }
        }

        // Handle single expression (no concatenation needed)
        if exprs.len() == 1 {
            return Ok(exprs.remove(0));
        }

        // Build a chain of concatenations: a + b + c + ...
        let mut result = exprs.remove(0);
        for expr in exprs {
            result = Expr {
                kind: ExprKind::Binary(Box::new(result), BinOp::Add, Box::new(expr)),
                span,
            };
        }

        Ok(result)
    }

    /// Parse an expression embedded in an f-string.
    fn parse_embedded_expr(&mut self, expr_src: &str, span: Span) -> Result<Expr> {
        // Lex and parse the expression source
        use crate::lexer::Scanner;

        let scanner = Scanner::new(expr_src);
        let (tokens, lex_errors) = scanner.scan_all();

        // Check for lexer errors
        if !lex_errors.is_empty() {
            return Err(ParseError::new(
                format!("error in f-string expression: {}", lex_errors[0].message),
                span
            ).into());
        }

        // Filter out error tokens
        let mut clean_tokens = Vec::new();
        for tok in tokens {
            if let TokenKind::Error(msg) = &tok.kind {
                return Err(ParseError::new(
                    format!("error in f-string expression: {}", msg),
                    span
                ).into());
            }
            clean_tokens.push(tok);
        }

        // Parse the expression
        let mut parser = Parser::new(&clean_tokens);
        parser.parse_expr()
    }

    fn parse_if_expr(&mut self, start: Span) -> Result<Expr> {
        let condition = self.parse_expr()?;

        let (then_branch, else_branch) = if self.match_token(TokenKind::Then) {
            // Inline if: if cond then a else b
            // But allow multi-line then branch with indented block
            let then_expr = if self.check(TokenKind::Newline) {
                self.advance();
                if self.check(TokenKind::Indent) {
                    let block = self.parse_indent_block()?;
                    Expr {
                        kind: ExprKind::Block(block.clone()),
                        span: block.span,
                    }
                } else {
                    return Err(self.error("expected expression or indented block after 'then'"));
                }
            } else {
                self.parse_expr()?
            };
            // Skip newlines before else - it can be on the next line
            self.skip_newlines();
            self.expect(TokenKind::Else)?;
            // Also allow multi-line else branch
            let else_expr = if self.check(TokenKind::Newline) {
                self.advance();
                if self.check(TokenKind::Indent) {
                    let block = self.parse_indent_block()?;
                    Expr {
                        kind: ExprKind::Block(block.clone()),
                        span: block.span,
                    }
                } else {
                    return Err(self.error("expected expression or indented block after 'else'"));
                }
            } else {
                self.parse_expr()?
            };
            (
                IfBranch::Expr(Box::new(then_expr)),
                Some(ElseBranch::Expr(Box::new(else_expr))),
            )
        } else {
            // Block if
            let then_block = self.parse_block()?;
            let else_branch = if self.check(TokenKind::Else) {
                self.advance();
                if self.check(TokenKind::If) {
                    self.advance();
                    let else_if = self.parse_if_expr(self.current_span())?;
                    match else_if.kind {
                        ExprKind::If(if_expr) => Some(ElseBranch::ElseIf(if_expr)),
                        _ => unreachable!(),
                    }
                } else {
                    Some(ElseBranch::Block(self.parse_block()?))
                }
            } else {
                None
            };
            (IfBranch::Block(then_block), else_branch)
        };

        Ok(Expr {
            kind: ExprKind::If(Box::new(IfExpr {
                condition,
                then_branch,
                else_branch,
                span: start.merge(self.previous_span()),
            })),
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_match_expr(&mut self) -> Result<Expr> {
        let start = self.current_span();
        self.expect(TokenKind::M)?;
        let scrutinee = self.parse_expr()?;

        let arms = if self.check(TokenKind::LBrace) {
            self.advance();
            let arms = self.parse_match_arms_brace()?;
            self.expect(TokenKind::RBrace)?;
            arms
        } else if self.check(TokenKind::Newline) {
            self.advance();
            if self.check(TokenKind::Indent) {
                self.advance();
                self.parse_match_arms_indented()?
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };

        Ok(Expr {
            kind: ExprKind::Match(Box::new(scrutinee), arms),
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_match_arms_brace(&mut self) -> Result<Vec<MatchArm>> {
        let mut arms = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.at_end() {
            self.skip_newlines();
            if self.check(TokenKind::RBrace) {
                break;
            }
            arms.push(self.parse_match_arm()?);
            self.skip_newlines();
        }
        Ok(arms)
    }

    fn parse_match_arms_indented(&mut self) -> Result<Vec<MatchArm>> {
        let mut arms = Vec::new();
        while !self.check(TokenKind::Dedent) && !self.at_end() {
            self.skip_newlines();
            if self.check(TokenKind::Dedent) {
                break;
            }
            arms.push(self.parse_match_arm()?);
            self.skip_newlines();
        }
        if self.check(TokenKind::Dedent) {
            self.advance();
        }
        Ok(arms)
    }

    fn parse_match_arm(&mut self) -> Result<MatchArm> {
        let start = self.current_span();
        let pattern = self.parse_pattern()?;

        let guard = if self.match_token(TokenKind::If) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.expect(TokenKind::Arrow)?;

        // Check if body is on a new line (multi-line arm body)
        let body = if self.check(TokenKind::Newline) {
            self.advance(); // consume newline
            if self.check(TokenKind::Indent) {
                // Parse indented block as the arm body
                let block = self.parse_indent_block()?;
                Expr {
                    kind: ExprKind::Block(block.clone()),
                    span: block.span,
                }
            } else {
                return Err(self.error("expected indented block after -> on new line"));
            }
        } else {
            self.parse_expr()?
        };

        Ok(MatchArm {
            pattern,
            guard,
            body,
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_for_expr(&mut self, start: Span) -> Result<Expr> {
        let pattern = self.parse_pattern()?;
        self.expect(TokenKind::In)?;
        let iter = self.parse_expr()?;
        let block = self.parse_block()?;

        Ok(Expr {
            kind: ExprKind::For(pattern, Box::new(iter), block),
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_while_expr(&mut self) -> Result<Expr> {
        let start = self.current_span();
        self.expect(TokenKind::Wh)?;

        // Check for while-let: wh Some(x) = iter.next
        // This is tricky to detect, so for now just parse condition
        let condition = self.parse_expr()?;
        let block = self.parse_block()?;

        Ok(Expr {
            kind: ExprKind::While(Box::new(condition), block),
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_loop_expr(&mut self) -> Result<Expr> {
        let start = self.current_span();
        self.expect(TokenKind::Lp)?;
        let block = self.parse_block()?;

        Ok(Expr {
            kind: ExprKind::Loop(block),
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_closure(&mut self) -> Result<Expr> {
        let start = self.current_span();

        let params = if self.match_token(TokenKind::PipePipe) {
            Vec::new()
        } else {
            self.expect(TokenKind::Pipe)?;
            let mut params = Vec::new();
            if !self.check(TokenKind::Pipe) {
                loop {
                    let param_start = self.current_span();
                    let name = self.parse_ident()?;
                    let ty = if self.match_token(TokenKind::Colon) {
                        Some(self.parse_type()?)
                    } else {
                        None
                    };
                    params.push(ClosureParam {
                        name,
                        ty,
                        span: param_start.merge(self.previous_span()),
                    });
                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                }
            }
            self.expect(TokenKind::Pipe)?;
            params
        };

        let return_type = self.parse_optional_return_type()?;
        let body = self.parse_expr()?;

        Ok(Expr {
            kind: ExprKind::Closure(Closure {
                params,
                return_type,
                body: Box::new(body),
                span: start.merge(self.previous_span()),
            }),
            span: start.merge(self.previous_span()),
        })
    }

    fn try_parse_op_shorthand(&mut self) -> Result<Option<Expr>> {
        let start = self.current_span();

        // (+ 10), (> 0), etc.
        let op = if self.check(TokenKind::Plus) {
            Some(BinOp::Add)
        } else if self.check(TokenKind::Minus) {
            Some(BinOp::Sub)
        } else if self.check(TokenKind::Star) {
            Some(BinOp::Mul)
        } else if self.check(TokenKind::Slash) {
            Some(BinOp::Div)
        } else if self.check(TokenKind::Percent) {
            Some(BinOp::Mod)
        } else if self.check(TokenKind::EqEq) {
            Some(BinOp::Eq)
        } else if self.check(TokenKind::BangEq) {
            Some(BinOp::Ne)
        } else if self.check(TokenKind::Lt) {
            Some(BinOp::Lt)
        } else if self.check(TokenKind::LtEq) {
            Some(BinOp::Le)
        } else if self.check(TokenKind::Gt) {
            Some(BinOp::Gt)
        } else if self.check(TokenKind::GtEq) {
            Some(BinOp::Ge)
        } else {
            None
        };

        if let Some(op) = op {
            self.advance();
            let operand = self.parse_expr()?;
            self.expect(TokenKind::RParen)?;
            return Ok(Some(Expr {
                kind: ExprKind::OpShorthand(op, Box::new(operand), false), // op on right
                span: start.merge(self.previous_span()),
            }));
        }

        Ok(None)
    }

    fn parse_map_or_set(&mut self, start: Span) -> Result<Expr> {
        if self.match_token(TokenKind::RBrace) {
            // Empty map/set - default to empty map
            return Ok(Expr {
                kind: ExprKind::MapOrSet(Vec::new()),
                span: start.merge(self.previous_span()),
            });
        }

        let first_key = self.parse_expr()?;

        if self.match_token(TokenKind::Colon) {
            // Map literal
            let first_value = self.parse_expr()?;
            let mut entries = vec![MapEntry {
                key: first_key,
                value: Some(first_value),
                span: start.merge(self.previous_span()),
            }];

            while self.match_token(TokenKind::Comma) {
                if self.check(TokenKind::RBrace) {
                    break;
                }
                let key = self.parse_expr()?;
                self.expect(TokenKind::Colon)?;
                let value = self.parse_expr()?;
                entries.push(MapEntry {
                    key,
                    value: Some(value),
                    span: start.merge(self.previous_span()),
                });
            }

            self.expect(TokenKind::RBrace)?;
            return Ok(Expr {
                kind: ExprKind::MapOrSet(entries),
                span: start.merge(self.previous_span()),
            });
        }

        // Set literal
        let mut entries = vec![MapEntry {
            key: first_key,
            value: None,
            span: start.merge(self.previous_span()),
        }];

        while self.match_token(TokenKind::Comma) {
            if self.check(TokenKind::RBrace) {
                break;
            }
            let key = self.parse_expr()?;
            entries.push(MapEntry {
                key,
                value: None,
                span: start.merge(self.previous_span()),
            });
        }

        self.expect(TokenKind::RBrace)?;
        Ok(Expr {
            kind: ExprKind::MapOrSet(entries),
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_struct_expr(&mut self, name: Ident, start: Span) -> Result<Expr> {
        self.expect(TokenKind::LBrace)?;
        // Skip newlines and indentation - they're not significant inside braces
        self.skip_whitespace_tokens();

        let mut fields = Vec::new();
        let mut base = None;

        if !self.check(TokenKind::RBrace) {
            loop {
                // Check for struct base: ..expr
                if self.match_token(TokenKind::DotDot) {
                    base = Some(Box::new(self.parse_expr()?));
                    self.skip_whitespace_tokens();
                    break;
                }

                let field_start = self.current_span();
                let field_name = self.parse_ident()?;
                let value = if self.match_token(TokenKind::Colon) {
                    Some(self.parse_expr()?)
                } else {
                    None
                };

                fields.push(FieldInit {
                    name: field_name,
                    value,
                    span: field_start.merge(self.previous_span()),
                });

                // Allow comma or newline as field separator
                let had_comma = self.match_token(TokenKind::Comma);
                self.skip_whitespace_tokens();

                if self.check(TokenKind::RBrace) {
                    break;
                }

                // If no comma and we're not at RBrace, the newline acts as separator
                if !had_comma && !self.check_ident() && !self.check(TokenKind::DotDot) {
                    break;
                }
            }
        }

        self.expect(TokenKind::RBrace)?;

        let path = TypePath {
            segments: vec![TypePathSegment {
                name,
                args: None,
                span: start,
            }],
            span: start,
        };

        Ok(Expr {
            kind: ExprKind::Struct(path, fields, base),
            span: start.merge(self.previous_span()),
        })
    }

    // ========================================================================
    // Patterns
    // ========================================================================

    fn parse_pattern(&mut self) -> Result<Pattern> {
        let start = self.current_span();
        let mut pattern = self.parse_pattern_primary()?;

        // Or pattern: A | B | C
        if self.check(TokenKind::Pipe) {
            let mut patterns = vec![pattern];
            while self.match_token(TokenKind::Pipe) {
                patterns.push(self.parse_pattern_primary()?);
            }
            pattern = Pattern {
                kind: PatternKind::Or(patterns),
                span: start.merge(self.previous_span()),
            };
        }

        Ok(pattern)
    }

    fn parse_pattern_primary(&mut self) -> Result<Pattern> {
        let start = self.current_span();

        // Wildcard: _
        if self.check_ident_str("_") {
            self.advance();
            return Ok(Pattern {
                kind: PatternKind::Wildcard,
                span: start.merge(self.previous_span()),
            });
        }

        // Rest pattern: ..
        if self.match_token(TokenKind::DotDot) {
            return Ok(Pattern {
                kind: PatternKind::Rest,
                span: start.merge(self.previous_span()),
            });
        }

        // Reference pattern: &pat or &mut pat
        if self.match_token(TokenKind::Amp) {
            let is_mut = self.match_token(TokenKind::Mut);
            let inner = self.parse_pattern_primary()?;
            return Ok(Pattern {
                kind: PatternKind::Ref(Box::new(inner), is_mut),
                span: start.merge(self.previous_span()),
            });
        }

        // Literal pattern (including negative numbers)
        if self.check(TokenKind::Minus) {
            self.advance();
            if let Some(lit) = self.try_parse_literal()? {
                return Ok(Pattern {
                    kind: PatternKind::Literal(Literal {
                        kind: match lit.kind {
                            LiteralKind::Int(n) => LiteralKind::Int(-n),
                            LiteralKind::Float(n) => LiteralKind::Float(-n),
                            _ => return Err(self.error("expected numeric literal after '-'")),
                        },
                        span: start.merge(self.previous_span()),
                    }),
                    span: start.merge(self.previous_span()),
                });
            }
        }

        // Literal pattern
        if let Some(lit) = self.try_parse_literal()? {
            // Check for range pattern
            if self.check(TokenKind::DotDot) || self.check(TokenKind::DotDotEq) {
                let inclusive = self.match_token(TokenKind::DotDotEq);
                if !inclusive {
                    self.expect(TokenKind::DotDot)?;
                }
                let start_pat = Pattern {
                    kind: PatternKind::Literal(lit),
                    span: start,
                };
                let end = if !self.check(TokenKind::Arrow)
                    && !self.check(TokenKind::If)
                    && !self.check(TokenKind::Pipe)
                {
                    Some(Box::new(self.parse_pattern_primary()?))
                } else {
                    None
                };
                return Ok(Pattern {
                    kind: PatternKind::Range(Some(Box::new(start_pat)), end, inclusive),
                    span: start.merge(self.previous_span()),
                });
            }

            return Ok(Pattern {
                kind: PatternKind::Literal(lit),
                span: start.merge(self.previous_span()),
            });
        }

        // Tuple pattern: (a, b, c)
        if self.match_token(TokenKind::LParen) {
            let mut patterns = Vec::new();
            if !self.check(TokenKind::RParen) {
                loop {
                    patterns.push(self.parse_pattern()?);
                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                    if self.check(TokenKind::RParen) {
                        break;
                    }
                }
            }
            self.expect(TokenKind::RParen)?;
            return Ok(Pattern {
                kind: PatternKind::Tuple(patterns),
                span: start.merge(self.previous_span()),
            });
        }

        // List pattern: [a, b, ..rest]
        if self.match_token(TokenKind::LBracket) {
            let mut patterns = Vec::new();
            let mut rest = None;

            if !self.check(TokenKind::RBracket) {
                loop {
                    if self.check(TokenKind::DotDot) {
                        self.advance();
                        if self.check_ident() {
                            rest = Some(Box::new(self.parse_pattern_primary()?));
                        }
                        break;
                    }
                    patterns.push(self.parse_pattern()?);
                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                    if self.check(TokenKind::RBracket) {
                        break;
                    }
                }
            }
            self.expect(TokenKind::RBracket)?;
            return Ok(Pattern {
                kind: PatternKind::List(patterns, rest),
                span: start.merge(self.previous_span()),
            });
        }

        // mut binding or identifier
        let is_mut = self.match_token(TokenKind::Mut);

        // Identifier pattern (possibly with struct destructure)
        if self.check_ident() {
            let name = self.parse_ident()?;

            // Check for struct pattern: Point(x, y) or Point { x, y }
            if self.check(TokenKind::LParen) {
                return self.parse_struct_pattern(name, start);
            }

            // Check for @ binding
            let binding = if self.match_token(TokenKind::At) {
                Some(Box::new(self.parse_pattern_primary()?))
            } else {
                None
            };

            // Check for range
            if self.check(TokenKind::DotDot) || self.check(TokenKind::DotDotEq) {
                let inclusive = self.match_token(TokenKind::DotDotEq);
                if !inclusive {
                    self.expect(TokenKind::DotDot)?;
                }
                let start_pat = Pattern {
                    kind: PatternKind::Ident(name, is_mut, None),
                    span: start,
                };
                let end = if !self.check(TokenKind::Arrow)
                    && !self.check(TokenKind::If)
                    && !self.check(TokenKind::Pipe)
                {
                    Some(Box::new(self.parse_pattern_primary()?))
                } else {
                    None
                };
                return Ok(Pattern {
                    kind: PatternKind::Range(Some(Box::new(start_pat)), end, inclusive),
                    span: start.merge(self.previous_span()),
                });
            }

            return Ok(Pattern {
                kind: PatternKind::Ident(name, is_mut, binding),
                span: start.merge(self.previous_span()),
            });
        }

        Err(self.error("expected pattern"))
    }

    fn parse_struct_pattern(&mut self, name: Ident, start: Span) -> Result<Pattern> {
        self.expect(TokenKind::LParen)?;

        let mut fields = Vec::new();
        let mut has_rest = false;

        if !self.check(TokenKind::RParen) {
            loop {
                if self.match_token(TokenKind::DotDot) {
                    has_rest = true;
                    break;
                }

                let field_start = self.current_span();
                let field_name = self.parse_ident()?;
                let pattern = if self.match_token(TokenKind::Colon) {
                    Some(self.parse_pattern()?)
                } else {
                    None
                };

                fields.push(PatternField {
                    name: field_name,
                    pattern,
                    span: field_start.merge(self.previous_span()),
                });

                if !self.match_token(TokenKind::Comma) {
                    break;
                }
                if self.check(TokenKind::RParen) {
                    break;
                }
            }
        }

        self.expect(TokenKind::RParen)?;

        let path = TypePath {
            segments: vec![TypePathSegment {
                name,
                args: None,
                span: start,
            }],
            span: start,
        };

        Ok(Pattern {
            kind: PatternKind::Struct(path, fields, has_rest),
            span: start.merge(self.previous_span()),
        })
    }

    // ========================================================================
    // Blocks
    // ========================================================================

    fn parse_block(&mut self) -> Result<Block> {
        if self.check(TokenKind::LBrace) {
            self.parse_brace_block()
        } else if self.check(TokenKind::Newline) {
            self.advance();
            if self.check(TokenKind::Indent) {
                self.parse_indent_block()
            } else {
                Ok(Block {
                    stmts: Vec::new(),
                    span: self.current_span(),
                })
            }
        } else {
            // Single expression as block
            let expr = self.parse_expr()?;
            Ok(Block {
                stmts: vec![Stmt {
                    kind: StmtKind::Expr(expr.clone()),
                    span: expr.span,
                }],
                span: expr.span,
            })
        }
    }

    fn parse_brace_block(&mut self) -> Result<Block> {
        let start = self.current_span();
        self.expect(TokenKind::LBrace)?;

        let mut stmts = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.at_end() {
            self.skip_newlines();
            if self.check(TokenKind::RBrace) {
                break;
            }
            stmts.push(self.parse_stmt()?);
            // Allow semicolons
            while self.match_token(TokenKind::Semicolon) || self.match_token(TokenKind::Newline) {}
        }

        self.expect(TokenKind::RBrace)?;

        Ok(Block {
            stmts,
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_indent_block(&mut self) -> Result<Block> {
        let start = self.current_span();
        self.expect(TokenKind::Indent)?;

        let mut stmts = Vec::new();
        while !self.check(TokenKind::Dedent) && !self.at_end() {
            self.skip_newlines();
            if self.check(TokenKind::Dedent) {
                break;
            }
            stmts.push(self.parse_stmt()?);
            self.skip_newlines();
        }

        if self.check(TokenKind::Dedent) {
            self.advance();
        }

        Ok(Block {
            stmts,
            span: start.merge(self.previous_span()),
        })
    }

    fn parse_stmt(&mut self) -> Result<Stmt> {
        let start = self.current_span();

        // Check for items. Single-letter keywords (f, s, e, t, i, m) can also be variable names,
        // so we need to distinguish:
        // - "s MyStruct" -> struct declaration (keyword followed by identifier = item name)
        // - "s := 42" or "s = 42" -> variable binding
        // - "s" alone -> variable reference
        // Multi-word keywords (Type, Us, Md) and attributes (@, Pub) are unambiguous.
        let is_unambiguous_item = self.check(TokenKind::At)
            || self.check(TokenKind::Pub)
            || self.check(TokenKind::Type)
            || self.check(TokenKind::Us)
            || self.check(TokenKind::Md);

        // Single-letter keywords need lookahead: item if followed by an identifier (the item name)
        let is_single_letter_keyword = self.check(TokenKind::F)
            || self.check(TokenKind::S)
            || self.check(TokenKind::E)
            || self.check(TokenKind::T)
            || self.check(TokenKind::I);

        // For single-letter keywords, check if next token is an identifier or type params
        // f name(...) - function, f<T> - generic function
        // s Name - struct
        // But NOT: s := 42, s = 42, s (alone), s + 1
        let is_item = is_unambiguous_item
            || (is_single_letter_keyword && self.next_token_is_item_name());

        if is_item {
            let item = self.parse_item()?;
            return Ok(Stmt {
                kind: StmtKind::Item(item),
                span: start.merge(self.previous_span()),
            });
        }

        // Parse expression (which might be an assignment)
        let expr = self.parse_expr()?;

        // Check if this is a let statement (assignment at statement level)
        // Only convert to let if the target is a valid pattern (identifiers, tuples, etc.)
        // For things like *x = 1 (deref assignment), keep as expression
        if let ExprKind::Assign(ref target, ref value, mutable) = expr.kind {
            // Try to convert to pattern - if it fails, keep as expression
            if let Ok(pattern) = self.expr_to_pattern(target) {
                return Ok(Stmt {
                    kind: StmtKind::Let(LetStmt {
                        pattern,
                        ty: None,
                        init: value.as_ref().clone(),
                        mutable,
                        span: start.merge(self.previous_span()),
                    }),
                    span: start.merge(self.previous_span()),
                });
            }
        }

        Ok(Stmt {
            kind: StmtKind::Expr(expr),
            span: start.merge(self.previous_span()),
        })
    }

    fn expr_to_pattern(&self, expr: &Expr) -> Result<Pattern> {
        let span = expr.span;
        match &expr.kind {
            ExprKind::Ident(name) => Ok(Pattern {
                kind: PatternKind::Ident(name.clone(), false, None),
                span,
            }),
            ExprKind::Tuple(elements) => {
                let patterns = elements
                    .iter()
                    .map(|e| self.expr_to_pattern(e))
                    .collect::<Result<Vec<_>>>()?;
                Ok(Pattern {
                    kind: PatternKind::Tuple(patterns),
                    span,
                })
            }
            _ => Err(ParseError::new("invalid pattern in assignment", span).into()),
        }
    }

    // ========================================================================
    // Helpers
    // ========================================================================

    fn try_parse_literal(&mut self) -> Result<Option<Literal>> {
        let start = self.current_span();

        let kind = match self.current_kind() {
            Some(TokenKind::Int(n)) => {
                self.advance();
                Some(LiteralKind::Int(n))
            }
            Some(TokenKind::Float(n)) => {
                self.advance();
                Some(LiteralKind::Float(n))
            }
            Some(TokenKind::String(ref s)) => {
                let s = s.clone();
                self.advance();
                Some(LiteralKind::String(s))
            }
            Some(TokenKind::Char(c)) => {
                self.advance();
                Some(LiteralKind::Char(c))
            }
            Some(TokenKind::True) => {
                self.advance();
                Some(LiteralKind::Bool(true))
            }
            Some(TokenKind::False) => {
                self.advance();
                Some(LiteralKind::Bool(false))
            }
            Some(TokenKind::None) => {
                self.advance();
                Some(LiteralKind::None)
            }
            _ => None,
        };

        Ok(kind.map(|kind| Literal {
            kind,
            span: start.merge(self.previous_span()),
        }))
    }

    fn parse_literal(&mut self) -> Result<Literal> {
        self.try_parse_literal()?
            .ok_or_else(|| self.error("expected literal"))
    }

    fn parse_ident(&mut self) -> Result<Ident> {
        let span = self.current_span();
        let lexeme = self.current().map(|t| t.lexeme.clone());
        match self.current_kind() {
            Some(TokenKind::Ident(ref name)) => {
                let name = name.clone();
                self.advance();
                Ok(Ident { name, span })
            }
            // Allow keywords as identifiers in certain contexts
            // Use the original lexeme to preserve case (T vs t, etc.)
            Some(TokenKind::I)
            | Some(TokenKind::T)
            | Some(TokenKind::S)
            | Some(TokenKind::E)
            | Some(TokenKind::M)
            | Some(TokenKind::F)
            // T and F are also lexed as True/False, but can be type params
            | Some(TokenKind::True)
            | Some(TokenKind::False)
            | Some(TokenKind::None)
            // Builtin constructors can also be identifiers (enum variants, etc.)
            | Some(TokenKind::Some)
            | Some(TokenKind::Ok)
            | Some(TokenKind::Err) => {
                self.advance();
                Ok(Ident {
                    name: lexeme.unwrap_or_default(),
                    span,
                })
            }
            _ => Err(self.error("expected identifier")),
        }
    }

    fn check_ident(&mut self) -> bool {
        matches!(
            self.current_kind(),
            Some(TokenKind::Ident(_))
                | Some(TokenKind::I)
                | Some(TokenKind::T)
                | Some(TokenKind::S)
                | Some(TokenKind::E)
                | Some(TokenKind::M)
                | Some(TokenKind::F)
                | Some(TokenKind::True)
                | Some(TokenKind::False)
                | Some(TokenKind::None)
                | Some(TokenKind::Some)
                | Some(TokenKind::Ok)
                | Some(TokenKind::Err)
        )
    }

    fn check_ident_str(&mut self, s: &str) -> bool {
        match self.current_kind() {
            Some(TokenKind::Ident(ref name)) => name == s,
            _ => false,
        }
    }

    fn check_expr_start(&mut self) -> bool {
        self.check_ident()
            || matches!(
                self.current_kind(),
                Some(TokenKind::Int(_))
                    | Some(TokenKind::Float(_))
                    | Some(TokenKind::String(_))
                    | Some(TokenKind::Char(_))
                    | Some(TokenKind::True)
                    | Some(TokenKind::False)
                    | Some(TokenKind::None)
                    | Some(TokenKind::LParen)
                    | Some(TokenKind::LBracket)
                    | Some(TokenKind::LBrace)
                    | Some(TokenKind::Pipe)
                    | Some(TokenKind::PipePipe)
                    | Some(TokenKind::Dot)
                    | Some(TokenKind::Amp)
                    | Some(TokenKind::Star)
                    | Some(TokenKind::Minus)
                    | Some(TokenKind::Bang)
                    | Some(TokenKind::If)
                    | Some(TokenKind::M)
                    | Some(TokenKind::For)
                    | Some(TokenKind::Wh)
                    | Some(TokenKind::Lp)
                    | Some(TokenKind::Ret)
                    | Some(TokenKind::Br)
                    | Some(TokenKind::Ct)
                    | Some(TokenKind::Aw)
                    | Some(TokenKind::As)
                    | Some(TokenKind::Un)
            )
    }

    fn at_newline_boundary(&self) -> bool {
        if self.pos > 0 {
            matches!(self.tokens.get(self.pos - 1), Some(t) if t.kind == TokenKind::Newline)
        } else {
            false
        }
    }

    // Token helpers
    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn current_kind(&self) -> Option<TokenKind> {
        self.current().map(|t| t.kind.clone())
    }

    fn current_span(&self) -> Span {
        self.current().map(|t| t.span).unwrap_or_default()
    }

    fn previous_span(&self) -> Span {
        if self.pos > 0 {
            self.tokens[self.pos - 1].span
        } else {
            Span::default()
        }
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.current_kind() == Some(kind)
    }

    fn match_token(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn advance(&mut self) -> Option<&Token> {
        if !self.at_end() {
            self.pos += 1;
        }
        self.tokens.get(self.pos - 1)
    }

    fn at_end(&self) -> bool {
        matches!(self.current_kind(), Some(TokenKind::Eof) | None)
    }

    fn expect(&mut self, kind: TokenKind) -> Result<()> {
        if self.check(kind.clone()) {
            self.advance();
            Ok(())
        } else {
            Err(self.error(format!("expected {:?}", kind)))
        }
    }

    fn peek_is(&self, kind: TokenKind) -> bool {
        self.tokens
            .get(self.pos + 1)
            .is_some_and(|t| t.kind == kind)
    }

    /// Check if the next token (after current) looks like an item name.
    /// This is used to distinguish:
    /// - `s MyStruct` (struct declaration) from `s := 42` (variable binding) or `s` (variable ref)
    /// - `f foo(` (function) from `f := 42` or `f`
    fn next_token_is_item_name(&self) -> bool {
        match self.tokens.get(self.pos + 1).map(|t| &t.kind) {
            // Next token is an identifier - looks like an item name
            Some(TokenKind::Ident(_)) => true,
            // Next token is < for generic params like f<T>
            Some(TokenKind::Lt) => true,
            // Next token is ( for tuple struct like s Point(x, y) - wait that's not right
            // Actually `s Point(` would have Point as Ident first, so this is covered
            _ => false,
        }
    }

    fn skip_newlines(&mut self) {
        while self.check(TokenKind::Newline) {
            self.advance();
        }
    }

    /// Skip newlines, indents, and dedents. Used inside brace-delimited constructs
    /// where indentation is not significant.
    fn skip_whitespace_tokens(&mut self) {
        while self.check(TokenKind::Newline)
            || self.check(TokenKind::Indent)
            || self.check(TokenKind::Dedent)
        {
            self.advance();
        }
    }

    fn error(&self, message: impl Into<String>) -> crate::errors::CompileError {
        ParseError::new(message, self.current_span()).into()
    }
}
