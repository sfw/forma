//! LSP (Language Server Protocol) implementation for FORMA.
//!
//! This module provides IDE support for FORMA through the Language Server Protocol.

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::compiler::CompilerSession;
use crate::lexer::{Scanner, Span};
use crate::parser::{ItemKind, Parser};
use crate::semantic::SemanticIndex;

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
                            get_symbol_type_info(content, name, token.span.start)
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
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
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

            let offset = tokens.iter().find_map(|token| {
                let token_end = token.span.column + (token.span.end - token.span.start);
                (token.span.line == line && token.span.column <= col && col <= token_end)
                    .then_some(token.span.start)
            });
            let semantic_index = SemanticIndex::from_tokens(&tokens);
            if let Some(definition) = offset.and_then(|offset| semantic_index.definition_at(offset))
            {
                let location = Location {
                    uri: uri.clone(),
                    range: span_to_range(definition.span),
                };
                return Ok(Some(GotoDefinitionResponse::Scalar(location)));
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
                Ok(_) => {
                    let syntax = crate::syntax::LosslessSyntax::parse(content.clone());
                    let formatted = crate::syntax::LosslessFormatter::format(&syntax);

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

            let target_offset = tokens.iter().find_map(|token| {
                let token_end = token.span.column + (token.span.end - token.span.start);
                if token.span.line == line && token.span.column <= col && col <= token_end {
                    Some(token.span.start)
                } else {
                    None
                }
            });

            let semantic_index = SemanticIndex::from_tokens(&tokens);
            if let Some(definition) =
                target_offset.and_then(|offset| semantic_index.definition_at(offset))
            {
                let locations: Vec<Location> = semantic_index
                    .references_to(definition.id, params.context.include_declaration)
                    .into_iter()
                    .map(|span| Location {
                        uri: uri.clone(),
                        range: span_to_range(span),
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
        "toml_parse" => Some("toml_parse(s: Str) -> Result[Json, Str]\nParse TOML configuration".to_string()),
        "toml_stringify" => Some("toml_stringify(json: Json) -> Result[Str, Str]\nSerialize JSON-compatible data as TOML".to_string()),
        "json_stringify" => Some("json_stringify(json: Json) -> Str\nConvert JSON to string".to_string()),
        "file_read" => Some("file_read(path: Str) -> Result[Str, Str]\nRead a file to string".to_string()),
        "file_write" => Some("file_write(path: Str, content: Str) -> Result[(), Str]\nWrite string to file".to_string()),
        "http_get" => Some("http_get(url: Str) -> Result[(Int, Str, Map), Str]\nMake HTTP GET request".to_string()),
        "http_post" => Some("http_post(url: Str, body: Str) -> Result[(Int, Str, Map), Str]\nMake HTTP POST request".to_string()),
        "http_request" => Some("http_request(method: Str, url: Str, body: Str, headers: Map[Str], timeout_ms: Int, follow_redirects: Bool) -> Result[(Int, Str, Map), Str]\nMake a general authenticated HTTP request".to_string()),
        "db_connect_postgres" => Some("db_connect_postgres(url: Str) -> Result[Database, Str]\nConnect to remote PostgreSQL".to_string()),
        "tcp_connect" => Some("tcp_connect(host: Str, port: Int) -> Result[TcpStream, Str]\nConnect to TCP server".to_string()),
        "tcp_listen" => Some("tcp_listen(host: Str, port: Int) -> Result[TcpListener, Str]\nCreate TCP listener".to_string()),
        "alloc" => Some("alloc(size: Int) -> *Void\nAllocate memory".to_string()),
        "dealloc" => Some("dealloc(ptr: *Void, size: Int) -> ()\nFree memory".to_string()),
        // Sprint 51
        "map" => Some("map(arr: [T], fn: (T) -> U) -> [U]\nTransform each element of an array".to_string()),
        "filter" => Some("filter(arr: [T], fn: (T) -> Bool) -> [T]\nKeep elements matching predicate".to_string()),
        "reduce" => Some("reduce(arr: [T], init: U, fn: (U, T) -> U) -> U\nFold array with accumulator".to_string()),
        "any" => Some("any(arr: [T], fn: (T) -> Bool) -> Bool\nTest if any element matches".to_string()),
        "all" => Some("all(arr: [T], fn: (T) -> Bool) -> Bool\nTest if all elements match".to_string()),
        "vec_sort" => Some("vec_sort(arr: [T]) -> [T]\nSort array (Int, Float, Str, Char)".to_string()),
        "vec_index_of" => Some("vec_index_of(arr: [T], target: T) -> Int?\nFind index of first matching element".to_string()),
        "str_replace" => Some("str_replace(s: Str, pattern: Str, replacement: Str) -> Str\nReplace all occurrences".to_string()),
        "str_to_float" => Some("str_to_float(s: Str) -> Float?\nParse string to float".to_string()),
        "log2" => Some("log2(x: Float) -> Float\nBase-2 logarithm".to_string()),
        "asin" => Some("asin(x: Float) -> Float\nArcsine (inverse sine)".to_string()),
        "acos" => Some("acos(x: Float) -> Float\nArccosine (inverse cosine)".to_string()),
        "atan2" => Some("atan2(y: Float, x: Float) -> Float\nTwo-argument arctangent".to_string()),
        "map_opt" => Some("map_opt(opt: T?, fn: (T) -> U) -> U?\nMap over Option value".to_string()),
        "flatten" => Some("flatten(opt: T??) -> T?\nFlatten nested Option".to_string()),
        "and_then" => Some("and_then(opt: T?, fn: (T) -> U?) -> U?\nChain Option with function returning Option".to_string()),
        "file_read_bytes" => Some("file_read_bytes(path: Str) -> Result[[Int], Str]\nRead file as byte array".to_string()),
        "file_write_bytes" => Some("file_write_bytes(path: Str, bytes: [Int]) -> Result[(), Str]\nWrite byte array to file".to_string()),
        "random_shuffle" => Some("random_shuffle(arr: [T]) -> [T]\nShuffle array randomly".to_string()),
        _ => crate::builtins::metadata()
            .into_iter()
            .find(|builtin| builtin.name == name)
            .map(|metadata| {
            let effects = metadata
                .effects
                .iter()
                .map(|effect| format!("{effect:?}"))
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "{}: {}\n{}\nEffects: {}\nCapability: {}",
                metadata.name,
                metadata.signature,
                metadata.documentation,
                if effects.is_empty() { "none" } else { &effects },
                metadata.capability
                    .map_or("none", |capability| capability.as_str())
            )
        }),
    }
}

/// Get diagnostics for source content (extracted for testability).
pub fn analyze_diagnostics(content: &str) -> Vec<Diagnostic> {
    let mut session = CompilerSession::new();
    match session.compile_source("<lsp>", content) {
        Ok(_) => Vec::new(),
        Err(diagnostics) => diagnostics
            .into_iter()
            .map(|diagnostic| Diagnostic {
                range: span_to_range(diagnostic.span),
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String(diagnostic.phase.code().to_string())),
                source: Some("forma".to_string()),
                message: diagnostic.message,
                ..Default::default()
            })
            .collect(),
    }
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
        completions.extend(crate::lexer::KEYWORDS.iter().map(|keyword| {
            completion_item(
                keyword.canonical,
                CompletionItemKind::KEYWORD,
                &format!("{:?} keyword", keyword.keyword),
            )
        }));
        completions.extend(vec![
            completion_item("print", CompletionItemKind::FUNCTION, "Print to stdout"),
            completion_item(
                "println",
                CompletionItemKind::FUNCTION,
                "Print with newline",
            ),
        ]);
        let semantics = SemanticIndex::from_tokens(&tokens);
        completions.extend(semantics.definitions().iter().map(|definition| {
            let kind = match definition.kind {
                crate::semantic::SymbolKind::Function => CompletionItemKind::FUNCTION,
                crate::semantic::SymbolKind::Type => CompletionItemKind::CLASS,
                crate::semantic::SymbolKind::Module => CompletionItemKind::MODULE,
                crate::semantic::SymbolKind::Parameter | crate::semantic::SymbolKind::Local => {
                    CompletionItemKind::VARIABLE
                }
            };
            completion_item(&definition.name, kind, &format!("{:?}", definition.kind))
        }));
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
        // Sprint 51: New builtins
        completion_item(
            "map",
            CompletionItemKind::FUNCTION,
            "Transform array elements with closure",
        ),
        completion_item(
            "filter",
            CompletionItemKind::FUNCTION,
            "Filter array elements with predicate",
        ),
        completion_item(
            "reduce",
            CompletionItemKind::FUNCTION,
            "Fold array with accumulator",
        ),
        completion_item(
            "any",
            CompletionItemKind::FUNCTION,
            "Test if any element matches predicate",
        ),
        completion_item(
            "all",
            CompletionItemKind::FUNCTION,
            "Test if all elements match predicate",
        ),
        completion_item(
            "vec_sort",
            CompletionItemKind::FUNCTION,
            "Sort array (generic)",
        ),
        completion_item(
            "vec_index_of",
            CompletionItemKind::FUNCTION,
            "Find index of element",
        ),
        completion_item(
            "str_replace",
            CompletionItemKind::FUNCTION,
            "Replace all occurrences in string",
        ),
        completion_item(
            "str_to_float",
            CompletionItemKind::FUNCTION,
            "Parse string to float",
        ),
        completion_item(
            "map_opt",
            CompletionItemKind::FUNCTION,
            "Map over Option value",
        ),
        completion_item(
            "flatten",
            CompletionItemKind::FUNCTION,
            "Flatten nested Option",
        ),
        completion_item(
            "and_then",
            CompletionItemKind::FUNCTION,
            "Chain Option with function returning Option",
        ),
        completion_item("log2", CompletionItemKind::FUNCTION, "Base-2 logarithm"),
        completion_item("asin", CompletionItemKind::FUNCTION, "Arcsine"),
        completion_item("acos", CompletionItemKind::FUNCTION, "Arccosine"),
        completion_item(
            "atan2",
            CompletionItemKind::FUNCTION,
            "Two-argument arctangent",
        ),
        completion_item(
            "file_read_bytes",
            CompletionItemKind::FUNCTION,
            "Read file as byte array",
        ),
        completion_item(
            "file_write_bytes",
            CompletionItemKind::FUNCTION,
            "Write byte array to file",
        ),
        completion_item(
            "random_shuffle",
            CompletionItemKind::FUNCTION,
            "Shuffle array randomly",
        ),
    ]);

    for builtin in crate::builtins::all() {
        if !completions.iter().any(|item| item.label == builtin.name) {
            completions.push(completion_item(
                builtin.name,
                CompletionItemKind::FUNCTION,
                builtin.documentation,
            ));
        }
    }

    completions.sort_by(|left, right| left.label.cmp(&right.label));
    completions.dedup_by(|left, right| left.label == right.label);

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
fn get_symbol_type_info(content: &str, name: &str, offset: usize) -> Option<String> {
    let mut session = CompilerSession::new();
    let compilation = session.compile_source("<lsp>", content).ok()?;
    let definition = compilation.semantics.definition_at(offset);
    let kind_label = match definition.map(|definition| definition.kind) {
        Some(crate::semantic::SymbolKind::Function) => "function",
        Some(crate::semantic::SymbolKind::Type) => "type",
        Some(crate::semantic::SymbolKind::Module) => "module",
        Some(crate::semantic::SymbolKind::Parameter) => "parameter",
        Some(crate::semantic::SymbolKind::Local) => "variable",
        None => "symbol",
    };
    let ty = compilation.type_at_offset(offset).cloned().or_else(|| {
        compilation.program.functions.get(name).map(|function| {
            crate::types::Ty::Fn(
                function.params.iter().map(|(_, ty)| ty.clone()).collect(),
                Box::new(function.return_ty.clone()),
            )
        })
    })?;
    Some(format!("{} {}: {}", kind_label, name, ty))
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
    fn completions_include_semantically_indexed_user_symbols() {
        let source = "f helper() -> Int = 1\nf main() -> Int\n    local = helper()\n    ";
        let completions = analyze_completions(
            source,
            Position {
                line: 3,
                character: 4,
            },
        );
        let labels: Vec<_> = completions.iter().map(|item| item.label.as_str()).collect();
        assert!(labels.contains(&"helper"));
        assert!(labels.contains(&"local"));
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
