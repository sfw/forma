//! LSP (Language Server Protocol) implementation for FORMA.
//!
//! This module provides IDE support for FORMA through the Language Server Protocol.

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::borrow::BorrowChecker;
use crate::fmt::Formatter;
use crate::lexer::{Scanner, Span};
use crate::parser::{ItemKind, Parser};
use crate::types::TypeChecker;

/// Document state for tracking open files
#[derive(Debug, Clone)]
struct Document {
    content: String,
    #[allow(dead_code)]
    version: i32,
}

/// FORMA Language Server
pub struct FormaLanguageServer {
    client: Client,
    documents: Arc<RwLock<HashMap<Url, Document>>>,
}

impl FormaLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Analyze a document and publish diagnostics
    async fn analyze_document(&self, uri: &Url, content: &str) {
        let diagnostics = self.get_diagnostics(uri, content);
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }

    /// Get diagnostics for a document
    fn get_diagnostics(&self, _uri: &Url, content: &str) -> Vec<Diagnostic> {
        analyze_diagnostics(content)
    }

    /// Get completions at a position
    fn get_completions(&self, content: &str, position: Position) -> Vec<CompletionItem> {
        analyze_completions(content, position)
    }

    /// Get hover information at a position
    fn get_hover(&self, content: &str, position: Position) -> Option<Hover> {
        let scanner = Scanner::new(content);
        let (tokens, _) = scanner.scan_all();

        let line = position.line as usize + 1;
        let col = position.character as usize + 1;

        // Find token at position
        for token in &tokens {
            let token_end = token.span.column + (token.span.end - token.span.start);
            if token.span.line == line && token.span.column <= col && col <= token_end {
                let info = match &token.kind {
                    crate::lexer::TokenKind::Ident(name) => {
                        if let Some(info) = get_builtin_info(name) {
                            info
                        } else {
                            get_symbol_type_info(content, name)
                                .unwrap_or_else(|| format!("identifier: {}", name))
                        }
                    }
                    crate::lexer::TokenKind::Int(n) => format!("Int literal: {}", n),
                    crate::lexer::TokenKind::Float(n) => format!("Float literal: {}", n),
                    crate::lexer::TokenKind::String(s) => format!("Str literal: \"{}\"", s),
                    crate::lexer::TokenKind::Char(c) => format!("Char literal: '{}'", c),
                    crate::lexer::TokenKind::True | crate::lexer::TokenKind::False => {
                        "Bool".to_string()
                    }
                    crate::lexer::TokenKind::F => "keyword: function definition (f)".to_string(),
                    crate::lexer::TokenKind::S => "keyword: struct definition (s)".to_string(),
                    crate::lexer::TokenKind::E => "keyword: enum definition (e)".to_string(),
                    crate::lexer::TokenKind::T => "keyword: trait definition (t)".to_string(),
                    crate::lexer::TokenKind::I => "keyword: impl block (i)".to_string(),
                    crate::lexer::TokenKind::If => "keyword: conditional expression".to_string(),
                    crate::lexer::TokenKind::M => "keyword: pattern matching (m)".to_string(),
                    crate::lexer::TokenKind::Wh => "keyword: while loop (wh)".to_string(),
                    crate::lexer::TokenKind::For => "keyword: for loop".to_string(),
                    crate::lexer::TokenKind::Ret => {
                        "keyword: return from function (ret)".to_string()
                    }
                    _ => return None,
                };

                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("```forma\n{}\n```", info),
                    }),
                    range: Some(span_to_range(token.span)),
                });
            }
        }

        None
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for FormaLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                    retrigger_characters: None,
                    work_done_progress_options: Default::default(),
                }),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "forma-lsp".to_string(),
                version: Some("0.1.0".to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "FORMA Language Server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let content = params.text_document.text;
        let version = params.text_document.version;

        {
            let mut docs = self.documents.write().await;
            docs.insert(
                uri.clone(),
                Document {
                    content: content.clone(),
                    version,
                },
            );
        }

        self.analyze_document(&uri, &content).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = params.text_document.version;

        // Get the full content (we're using FULL sync)
        if let Some(change) = params.content_changes.into_iter().next() {
            let content = change.text;

            {
                let mut docs = self.documents.write().await;
                docs.insert(
                    uri.clone(),
                    Document {
                        content: content.clone(),
                        version,
                    },
                );
            }

            self.analyze_document(&uri, &content).await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;

        // Re-analyze on save
        let content = {
            let docs = self.documents.read().await;
            docs.get(&uri).map(|d| d.content.clone())
        };

        if let Some(content) = content {
            self.analyze_document(&uri, &content).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        let mut docs = self.documents.write().await;
        docs.remove(&uri);

        // Clear diagnostics
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let content = {
            let docs = self.documents.read().await;
            docs.get(&uri).map(|d| d.content.clone())
        };

        if let Some(content) = content {
            let completions = self.get_completions(&content, position);
            Ok(Some(CompletionResponse::Array(completions)))
        } else {
            Ok(None)
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let content = {
            let docs = self.documents.read().await;
            docs.get(&uri).map(|d| d.content.clone())
        };

        if let Some(content) = content {
            Ok(self.get_hover(&content, position))
        } else {
            Ok(None)
        }
    }

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
            let identifier_name = tokens
                .iter()
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
                    if type_checker.check(&ast).is_ok()
                        && let Some((def_span, _)) = type_checker.get_definition_location(&name)
                    {
                        let location = Location {
                            uri: uri.clone(),
                            range: span_to_range(def_span),
                        };
                        return Ok(Some(GotoDefinitionResponse::Scalar(location)));
                    }
                }
            }
        }

        Ok(None)
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri;

        let content = {
            let docs = self.documents.read().await;
            docs.get(&uri).map(|d| d.content.clone())
        };

        if let Some(content) = content {
            let scanner = Scanner::new(&content);
            let (tokens, lex_errors) = scanner.scan_all();

            if !lex_errors.is_empty() {
                return Ok(None);
            }

            let parser = Parser::new(&tokens);
            match parser.parse() {
                Ok(ast) => {
                    let mut formatter = Formatter::new();
                    let formatted = formatter.format(&ast);

                    let line_count = content.lines().count() as u32;
                    let last_line_len = content.lines().last().map_or(0, |l| l.len()) as u32;

                    Ok(Some(vec![TextEdit {
                        range: Range {
                            start: Position {
                                line: 0,
                                character: 0,
                            },
                            end: Position {
                                line: line_count,
                                character: last_line_len,
                            },
                        },
                        new_text: formatted,
                    }]))
                }
                Err(_) => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

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
            let symbols = analyze_document_symbols(&content, &uri);
            Ok(Some(DocumentSymbolResponse::Flat(symbols)))
        } else {
            Ok(None)
        }
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
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

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
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
}

/// Convert a FORMA span to an LSP range
fn span_to_range(span: Span) -> Range {
    Range {
        start: Position {
            line: (span.line.saturating_sub(1)) as u32,
            character: (span.column.saturating_sub(1)) as u32,
        },
        end: Position {
            line: (span.line.saturating_sub(1)) as u32,
            character: (span.column + (span.end - span.start).saturating_sub(1)) as u32,
        },
    }
}

/// Create a completion item
fn completion_item(label: &str, kind: CompletionItemKind, detail: &str) -> CompletionItem {
    CompletionItem {
        label: label.to_string(),
        kind: Some(kind),
        detail: Some(detail.to_string()),
        ..Default::default()
    }
}

/// Get documentation for builtin functions
fn get_builtin_info(name: &str) -> Option<String> {
    match name {
        "print" => Some("print(value: T) -> ()\nPrint a value to stdout".to_string()),
        "println" => Some("println(value: T) -> ()\nPrint a value to stdout with newline".to_string()),
        "vec_new" => Some("vec_new() -> [T]\nCreate a new empty vector".to_string()),
        "vec_push" => Some("vec_push(vec: [T], item: T) -> [T]\nAdd an item to a vector".to_string()),
        "vec_pop" => Some("vec_pop(vec: [T]) -> ([T], T?)\nRemove and return the last item".to_string()),
        "vec_len" => Some("vec_len(vec: [T]) -> Int\nGet the length of a vector".to_string()),
        "vec_get" => Some("vec_get(vec: [T], index: Int) -> T?\nGet an item by index".to_string()),
        "str_len" => Some("str_len(s: Str) -> Int\nGet the length of a string".to_string()),
        "str_split" => Some("str_split(s: Str, sep: Str) -> [Str]\nSplit a string by separator".to_string()),
        "str_contains" => Some("str_contains(s: Str, sub: Str) -> Bool\nCheck if string contains substring".to_string()),
        "map_new" => Some("map_new() -> Map\nCreate a new empty map".to_string()),
        "map_get" => Some("map_get(m: Map, key: Str) -> V?\nGet a value from a map".to_string()),
        "map_insert" => Some("map_insert(m: Map, key: Str, value: V) -> Map\nInsert a key-value pair".to_string()),
        "json_parse" => Some("json_parse(s: Str) -> Result[Json, Str]\nParse a JSON string".to_string()),
        "json_stringify" => Some("json_stringify(json: Json) -> Str\nConvert JSON to string".to_string()),
        "file_read" => Some("file_read(path: Str) -> Result[Str, Str]\nRead a file to string".to_string()),
        "file_write" => Some("file_write(path: Str, content: Str) -> Result[(), Str]\nWrite string to file".to_string()),
        "http_get" => Some("http_get(url: Str) -> Result[(Int, Str, Map), Str]\nMake HTTP GET request".to_string()),
        "http_post" => Some("http_post(url: Str, body: Str) -> Result[(Int, Str, Map), Str]\nMake HTTP POST request".to_string()),
        "tcp_connect" => Some("tcp_connect(host: Str, port: Int) -> Result[TcpStream, Str]\nConnect to TCP server".to_string()),
        "tcp_listen" => Some("tcp_listen(host: Str, port: Int) -> Result[TcpListener, Str]\nCreate TCP listener".to_string()),
        "alloc" => Some("alloc(size: Int) -> *Void\nAllocate memory".to_string()),
        "dealloc" => Some("dealloc(ptr: *Void, size: Int) -> ()\nFree memory".to_string()),
        _ => None,
    }
}

/// Get diagnostics for source content (extracted for testability).
pub fn analyze_diagnostics(content: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    let scanner = Scanner::new(content);
    let (tokens, lex_errors) = scanner.scan_all();

    for error in lex_errors {
        diagnostics.push(Diagnostic {
            range: span_to_range(error.span),
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String("LEX".to_string())),
            source: Some("forma".to_string()),
            message: error.message,
            ..Default::default()
        });
    }

    if !diagnostics.is_empty() {
        return diagnostics;
    }

    let parser = Parser::new(&tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errors) => {
            for e in errors {
                diagnostics.push(Diagnostic {
                    range: span_to_range(e.span()),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String("PARSE".to_string())),
                    source: Some("forma".to_string()),
                    message: format!("{}", e),
                    ..Default::default()
                });
            }
            return diagnostics;
        }
    };

    let mut type_checker = TypeChecker::new();
    if let Err(errors) = type_checker.check(&ast) {
        for error in errors {
            diagnostics.push(Diagnostic {
                range: span_to_range(error.span),
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String("TYPE".to_string())),
                source: Some("forma".to_string()),
                message: format!("{}", error),
                ..Default::default()
            });
        }
    }

    let mut borrow_checker = BorrowChecker::new();
    if let Err(errors) = borrow_checker.check(&ast) {
        for error in errors {
            diagnostics.push(Diagnostic {
                range: span_to_range(error.span),
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String("BORROW".to_string())),
                source: Some("forma".to_string()),
                message: format!("{}", error),
                ..Default::default()
            });
        }
    }

    diagnostics
}

/// Get completions for position in source content (extracted for testability).
pub fn analyze_completions(content: &str, position: Position) -> Vec<CompletionItem> {
    let mut completions = Vec::new();

    let scanner = Scanner::new(content);
    let (tokens, _) = scanner.scan_all();

    let line = position.line as usize + 1;
    let col = position.character as usize + 1;

    let mut prev_token_kind = None;
    for token in &tokens {
        if token.span.line == line && token.span.column <= col {
            // Skip Eof/Newline/Dedent so they don't overwrite the meaningful previous token
            if !matches!(
                token.kind,
                crate::lexer::TokenKind::Eof
                    | crate::lexer::TokenKind::Newline
                    | crate::lexer::TokenKind::Dedent
            ) {
                prev_token_kind = Some(format!("{:?}", token.kind));
            }
        }
    }

    let prev = prev_token_kind.as_deref().unwrap_or("");

    if prev.contains("Assign") || prev.contains("Eq") {
        completions.extend(vec![
            completion_item("if", CompletionItemKind::KEYWORD, "if expression"),
            completion_item("m", CompletionItemKind::KEYWORD, "match expression"),
            completion_item("true", CompletionItemKind::KEYWORD, "boolean true"),
            completion_item("false", CompletionItemKind::KEYWORD, "boolean false"),
        ]);
    } else if prev.contains("Arrow") {
        completions.extend(vec![
            completion_item("Int", CompletionItemKind::CLASS, "Integer type"),
            completion_item("Float", CompletionItemKind::CLASS, "Float type"),
            completion_item("Bool", CompletionItemKind::CLASS, "Boolean type"),
            completion_item("Str", CompletionItemKind::CLASS, "String type"),
            completion_item("Char", CompletionItemKind::CLASS, "Character type"),
            completion_item("[T]", CompletionItemKind::CLASS, "List type"),
            completion_item("T?", CompletionItemKind::CLASS, "Option type"),
            completion_item("Result[T, E]", CompletionItemKind::CLASS, "Result type"),
        ]);
    } else if prev.contains("Dot") {
        completions.extend(vec![
            completion_item("len", CompletionItemKind::METHOD, "Get length"),
            completion_item("push", CompletionItemKind::METHOD, "Add element"),
            completion_item("pop", CompletionItemKind::METHOD, "Remove last element"),
            completion_item("map", CompletionItemKind::METHOD, "Transform elements"),
            completion_item("filter", CompletionItemKind::METHOD, "Filter elements"),
        ]);
    } else {
        completions.extend(vec![
            completion_item("f", CompletionItemKind::KEYWORD, "Define function"),
            completion_item("s", CompletionItemKind::KEYWORD, "Define struct"),
            completion_item("e", CompletionItemKind::KEYWORD, "Define enum"),
            completion_item("t", CompletionItemKind::KEYWORD, "Define trait"),
            completion_item("impl", CompletionItemKind::KEYWORD, "Implement trait"),
            completion_item("if", CompletionItemKind::KEYWORD, "If expression"),
            completion_item("m", CompletionItemKind::KEYWORD, "Match expression"),
            completion_item("wh", CompletionItemKind::KEYWORD, "While loop"),
            completion_item("for", CompletionItemKind::KEYWORD, "For loop"),
            completion_item("ret", CompletionItemKind::KEYWORD, "Return statement"),
            completion_item("print", CompletionItemKind::FUNCTION, "Print to stdout"),
            completion_item(
                "println",
                CompletionItemKind::FUNCTION,
                "Print with newline",
            ),
        ]);
    }

    completions.extend(vec![
        completion_item("vec_new", CompletionItemKind::FUNCTION, "Create new vector"),
        completion_item("vec_push", CompletionItemKind::FUNCTION, "Push to vector"),
        completion_item("vec_len", CompletionItemKind::FUNCTION, "Get vector length"),
        completion_item("str_len", CompletionItemKind::FUNCTION, "Get string length"),
        completion_item("str_split", CompletionItemKind::FUNCTION, "Split string"),
        completion_item("map_new", CompletionItemKind::FUNCTION, "Create new map"),
        completion_item("map_get", CompletionItemKind::FUNCTION, "Get from map"),
        completion_item(
            "map_insert",
            CompletionItemKind::FUNCTION,
            "Insert into map",
        ),
    ]);

    completions
}

/// Extract document symbols from source content (extracted for testability).
#[allow(deprecated)] // SymbolInformation is deprecated in favor of DocumentSymbol but widely supported
pub fn analyze_document_symbols(content: &str, uri: &Url) -> Vec<SymbolInformation> {
    let scanner = Scanner::new(content);
    let (tokens, lex_errors) = scanner.scan_all();

    if !lex_errors.is_empty() {
        return vec![];
    }

    let parser = Parser::new(&tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(_) => return vec![],
    };

    let mut symbols = Vec::new();

    for item in &ast.items {
        let (name, kind) = match &item.kind {
            ItemKind::Function(f) => (f.name.name.clone(), SymbolKind::FUNCTION),
            ItemKind::Struct(s) => (s.name.name.clone(), SymbolKind::STRUCT),
            ItemKind::Enum(e) => (e.name.name.clone(), SymbolKind::ENUM),
            ItemKind::Trait(t) => (t.name.name.clone(), SymbolKind::INTERFACE),
            ItemKind::Const(c) => (c.name.name.clone(), SymbolKind::CONSTANT),
            ItemKind::TypeAlias(t) => (t.name.name.clone(), SymbolKind::TYPE_PARAMETER),
            _ => continue,
        };

        symbols.push(SymbolInformation {
            name,
            kind,
            location: Location {
                uri: uri.clone(),
                range: span_to_range(item.span),
            },
            tags: None,
            deprecated: None,
            container_name: None,
        });
    }

    symbols
}

/// Get signature help for a function call at the given position.
fn get_signature_help(content: &str, position: Position) -> Option<SignatureHelp> {
    let scanner = Scanner::new(content);
    let (tokens, _) = scanner.scan_all();

    let line = position.line as usize + 1;
    let col = position.character as usize + 1;

    // Walk tokens to find the function name before the nearest `(` at/before cursor,
    // and count commas to determine the active parameter.
    let mut fn_name = None;
    let mut active_param: u32 = 0;
    let mut paren_depth: i32 = 0;

    for (i, token) in tokens.iter().enumerate() {
        if token.span.line > line || (token.span.line == line && token.span.column > col) {
            break;
        }
        match &token.kind {
            crate::lexer::TokenKind::LParen => {
                paren_depth += 1;
                if i > 0
                    && let crate::lexer::TokenKind::Ident(name) = &tokens[i - 1].kind
                {
                    fn_name = Some(name.clone());
                    active_param = 0;
                }
            }
            crate::lexer::TokenKind::RParen => {
                paren_depth -= 1;
                if paren_depth <= 0 {
                    fn_name = None;
                }
            }
            crate::lexer::TokenKind::Comma => {
                if fn_name.is_some() && paren_depth > 0 {
                    active_param += 1;
                }
            }
            _ => {}
        }
    }

    let name = fn_name?;
    let info = get_builtin_info(&name)?;
    let sig_line = info.lines().next().unwrap_or(&info);

    Some(SignatureHelp {
        signatures: vec![SignatureInformation {
            label: sig_line.to_string(),
            documentation: Some(Documentation::String(info)),
            parameters: None,
            active_parameter: Some(active_param),
        }],
        active_signature: Some(0),
        active_parameter: Some(active_param),
    })
}

/// Try to get type information for a user-defined symbol from the type checker.
fn get_symbol_type_info(content: &str, name: &str) -> Option<String> {
    let scanner = Scanner::new(content);
    let (tokens, lex_errors) = scanner.scan_all();
    if !lex_errors.is_empty() {
        return None;
    }

    let parser = Parser::new(&tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(_) => return None,
    };

    let mut type_checker = TypeChecker::new();
    let _ = type_checker.check(&ast);

    let scheme = type_checker.env().get(name)?;
    let kind_label = if let Some((_span, def_kind)) = type_checker.get_definition_location(name) {
        match def_kind {
            crate::types::checker::DefinitionKind::Function => "function",
            crate::types::checker::DefinitionKind::Struct => "struct",
            crate::types::checker::DefinitionKind::Enum => "enum",
            crate::types::checker::DefinitionKind::Trait => "trait",
            crate::types::checker::DefinitionKind::TypeAlias => "type alias",
            crate::types::checker::DefinitionKind::Variable => "variable",
            crate::types::checker::DefinitionKind::Parameter => "parameter",
            crate::types::checker::DefinitionKind::EnumVariant => "variant",
        }
    } else {
        "symbol"
    };
    Some(format!("{} {}: {}", kind_label, name, scheme))
}

/// Run the LSP server
pub async fn run_server() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(FormaLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_diagnostics_valid_code() {
        let diagnostics = analyze_diagnostics("f main() -> Int = 42\n");
        assert!(
            diagnostics.is_empty(),
            "valid code should produce no diagnostics, got: {:?}",
            diagnostics
        );
    }

    #[test]
    fn test_diagnostics_syntax_error() {
        let diagnostics = analyze_diagnostics("f main( -> Int\n");
        assert!(
            !diagnostics.is_empty(),
            "syntax error should produce diagnostics"
        );
    }

    #[test]
    fn test_completions_default_keywords() {
        let completions = analyze_completions(
            "",
            Position {
                line: 0,
                character: 0,
            },
        );
        let labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        assert!(labels.contains(&"f"), "should contain 'f' keyword");
        assert!(labels.contains(&"if"), "should contain 'if' keyword");
        assert!(
            labels.contains(&"vec_new"),
            "should contain 'vec_new' builtin"
        );
    }

    #[test]
    fn test_document_symbols_function_and_struct() {
        let source = "f greet(name: Str) -> Str = name\n\ns Point\n    x: Int\n    y: Int\n";
        let uri = Url::parse("file:///test.forma").unwrap();
        let symbols = analyze_document_symbols(source, &uri);
        let names: Vec<&str> = symbols.iter().map(|s| s.name.as_str()).collect();
        assert!(names.contains(&"greet"), "should contain function 'greet'");
        assert!(names.contains(&"Point"), "should contain struct 'Point'");
        assert_eq!(symbols.len(), 2);
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
        assert_eq!(symbols[1].kind, SymbolKind::STRUCT);
    }

    #[test]
    fn test_document_symbols_empty() {
        let uri = Url::parse("file:///test.forma").unwrap();
        let symbols = analyze_document_symbols("", &uri);
        assert!(symbols.is_empty(), "empty source should produce no symbols");

        let symbols = analyze_document_symbols("f main( -> Int\n", &uri);
        assert!(
            symbols.is_empty(),
            "invalid source should produce no symbols"
        );
    }

    #[test]
    fn test_completions_after_arrow() {
        // Simulate content where previous token is Arrow (->)
        let source = "f main() -> ";
        let completions = analyze_completions(
            source,
            Position {
                line: 0,
                character: 12,
            },
        );
        let labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        assert!(labels.contains(&"Int"), "should contain Int type after ->");
        assert!(labels.contains(&"Str"), "should contain Str type after ->");
    }

    #[test]
    fn test_completions_after_dot() {
        let source = "x.";
        let completions = analyze_completions(
            source,
            Position {
                line: 0,
                character: 2,
            },
        );
        let labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        assert!(labels.contains(&"len"), "should contain 'len' after dot");
        assert!(labels.contains(&"push"), "should contain 'push' after dot");
    }

    #[test]
    fn test_diagnostics_type_error() {
        let source = "f add(a: Int, b: Int) -> Str = a + b\n";
        let diagnostics = analyze_diagnostics(source);
        assert!(
            !diagnostics.is_empty(),
            "type mismatch should produce diagnostics"
        );
        let has_type_diag = diagnostics
            .iter()
            .any(|d| d.code == Some(NumberOrString::String("TYPE".to_string())));
        assert!(has_type_diag, "should have TYPE diagnostic code");
    }
}
