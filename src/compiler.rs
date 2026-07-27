//! Shared compiler pipeline and source management.
//!
//! All user-facing compiler surfaces should compile through [`CompilerSession`]
//! so phase ordering and diagnostics cannot drift between the interpreter,
//! native backend, verifier, REPL, and language server.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::borrow::BorrowChecker;
use crate::lexer::{Scanner, Span};
use crate::mir::{self, Lowerer, OwnershipReport, Program};
use crate::module::ModuleLoader;
use crate::parser::{Parser, SourceFile};
use crate::semantic::SemanticIndex;
use crate::types::TypeChecker;

/// Stable identity of a source registered with a compiler session.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceId(u32);

impl SourceId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

/// Source text and its user-visible name.
#[derive(Debug, Clone)]
pub struct Source {
    pub name: String,
    pub path: Option<PathBuf>,
    pub text: String,
}

/// Session-owned source storage.
#[derive(Debug, Default)]
pub struct SourceMap {
    sources: Vec<Source>,
    paths: HashMap<PathBuf, SourceId>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_virtual(&mut self, name: impl Into<String>, text: impl Into<String>) -> SourceId {
        let id = SourceId(self.sources.len() as u32);
        self.sources.push(Source {
            name: name.into(),
            path: None,
            text: text.into(),
        });
        id
    }

    pub fn add_file(&mut self, path: &Path, text: impl Into<String>) -> SourceId {
        let normalized = normalize_path(path);
        if let Some(id) = self.paths.get(&normalized) {
            return *id;
        }

        let id = SourceId(self.sources.len() as u32);
        self.sources.push(Source {
            name: path.to_string_lossy().into_owned(),
            path: Some(path.to_path_buf()),
            text: text.into(),
        });
        self.paths.insert(normalized, id);
        id
    }

    pub fn get(&self, id: SourceId) -> Option<&Source> {
        self.sources.get(id.index())
    }

    pub fn source(&self, id: SourceId) -> Option<&str> {
        self.get(id).map(|source| source.text.as_str())
    }

    pub fn name(&self, id: SourceId) -> Option<&str> {
        self.get(id).map(|source| source.name.as_str())
    }

    pub fn len(&self) -> usize {
        self.sources.len()
    }

    pub fn is_empty(&self) -> bool {
        self.sources.is_empty()
    }
}

fn normalize_path(path: &Path) -> PathBuf {
    path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
}

/// Compiler phase that produced a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompilePhase {
    Io,
    Lex,
    Parse,
    Module,
    Type,
    Borrow,
    Lower,
    Ownership,
}

impl CompilePhase {
    pub fn code(self) -> &'static str {
        match self {
            Self::Io => "IO",
            Self::Lex => "LEX",
            Self::Parse => "PARSE",
            Self::Module => "MODULE",
            Self::Type => "TYPE",
            Self::Borrow => "BORROW",
            Self::Lower => "LOWER",
            Self::Ownership => "OWNERSHIP",
        }
    }
}

/// A source-aware diagnostic independent of a particular frontend renderer.
#[derive(Debug, Clone)]
pub struct CompilerDiagnostic {
    pub source_id: SourceId,
    pub phase: CompilePhase,
    pub span: Span,
    pub message: String,
    pub help: Option<String>,
}

impl CompilerDiagnostic {
    fn new(
        source_id: SourceId,
        phase: CompilePhase,
        span: Span,
        message: impl Into<String>,
    ) -> Self {
        Self {
            source_id,
            phase,
            span,
            message: message.into(),
            help: None,
        }
    }

    fn with_help(mut self, help: Option<String>) -> Self {
        self.help = help;
        self
    }
}

/// Successful output of the shared semantic pipeline.
#[derive(Debug)]
pub struct Compilation {
    pub source_id: SourceId,
    pub ast: SourceFile,
    pub program: Program,
    /// Ownership audit produced before optimization.
    pub ownership: OwnershipReport,
    /// Transitive effects inferred from the resolved MIR call graph.
    pub effects: crate::builtins::EffectReport,
    /// Lexically resolved symbols used by CLI and editor queries.
    pub semantics: SemanticIndex,
    /// Per-function interpreter/native/formal support, propagated through calls.
    pub backend_support: crate::support::BackendSupportReport,
    /// Finalized expression types keyed by source span.
    pub expression_types: HashMap<Span, crate::types::Ty>,
    /// Canonical modules participating in this compilation.
    pub modules: Vec<CompiledModule>,
}

impl Compilation {
    pub fn type_at_offset(&self, offset: usize) -> Option<&crate::types::Ty> {
        self.expression_types
            .iter()
            .filter(|(span, _)| span.start <= offset && offset <= span.end)
            .min_by_key(|(span, _)| span.end.saturating_sub(span.start))
            .map(|(_, ty)| ty)
    }
}

#[derive(Debug, Clone)]
pub struct CompiledModule {
    pub id: crate::module::ModuleId,
    pub source_id: SourceId,
    pub path: PathBuf,
    pub exports: Vec<String>,
}

/// Stateful compiler entry point shared by every consumer.
#[derive(Debug, Default)]
pub struct CompilerSession {
    sources: SourceMap,
}

impl CompilerSession {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn sources(&self) -> &SourceMap {
        &self.sources
    }

    pub fn compile_file(&mut self, path: &Path) -> Result<Compilation, Vec<CompilerDiagnostic>> {
        let text = match std::fs::read_to_string(path) {
            Ok(text) => text,
            Err(error) => {
                let id = self.sources.add_file(path, String::new());
                return Err(vec![CompilerDiagnostic::new(
                    id,
                    CompilePhase::Io,
                    Span::default(),
                    format!("failed to read file: {error}"),
                )]);
            }
        };
        let id = self.sources.add_file(path, text);
        match crate::package::PackageManifest::discover(path) {
            Ok(Some(manifest)) => {
                if let Err(error) = manifest.write_lockfile() {
                    return Err(vec![CompilerDiagnostic::new(
                        id,
                        CompilePhase::Io,
                        Span::default(),
                        error,
                    )]);
                }
            }
            Ok(None) => {}
            Err(error) => {
                return Err(vec![CompilerDiagnostic::new(
                    id,
                    CompilePhase::Module,
                    Span::default(),
                    error,
                )]);
            }
        }
        self.compile_registered(id)
    }

    pub fn compile_source(
        &mut self,
        name: impl Into<String>,
        text: impl Into<String>,
    ) -> Result<Compilation, Vec<CompilerDiagnostic>> {
        let id = self.sources.add_virtual(name, text);
        self.compile_registered(id)
    }

    /// Compile a registered source through the canonical semantic phase order.
    pub fn compile_registered(
        &mut self,
        source_id: SourceId,
    ) -> Result<Compilation, Vec<CompilerDiagnostic>> {
        // Compiler phases recursively walk source, type, and MIR trees. Run the
        // canonical pipeline on a known stack so callers such as Tokio/LSP and
        // Rust test workers do not inherit a platform-dependent small stack.
        // FORGE-RUST-GAP: FRG-014.
        std::thread::scope(|scope| {
            let worker = std::thread::Builder::new()
                .name("forma-compiler".to_string())
                .stack_size(8 * 1024 * 1024)
                .spawn_scoped(scope, || self.compile_registered_inner(source_id))
                .expect("failed to start Forma compiler worker");
            match worker.join() {
                Ok(result) => result,
                Err(payload) => std::panic::resume_unwind(payload),
            }
        })
    }

    fn compile_registered_inner(
        &mut self,
        source_id: SourceId,
    ) -> Result<Compilation, Vec<CompilerDiagnostic>> {
        let Some(source) = self.sources.get(source_id).cloned() else {
            return Err(vec![CompilerDiagnostic::new(
                source_id,
                CompilePhase::Io,
                Span::default(),
                "unknown source id",
            )]);
        };

        let scanner = Scanner::new(&source.text);
        let (tokens, lex_errors) = scanner.scan_all();
        if !lex_errors.is_empty() {
            return Err(lex_errors
                .into_iter()
                .map(|error| {
                    CompilerDiagnostic::new(source_id, CompilePhase::Lex, error.span, error.message)
                })
                .collect());
        }

        let parser = Parser::new(&tokens);
        let semantics = SemanticIndex::from_tokens(&tokens);
        let parsed = parser.parse().map_err(|errors| {
            errors
                .into_iter()
                .map(|error| {
                    let help = error.help().map(str::to_owned);
                    CompilerDiagnostic::new(
                        source_id,
                        CompilePhase::Parse,
                        error.span(),
                        error.to_string(),
                    )
                    .with_help(help)
                })
                .collect::<Vec<_>>()
        })?;

        let (ast, modules) = if let Some(path) = &source.path {
            let mut loader = ModuleLoader::from_source_file(path);
            let imported_items = loader.load_imports(&parsed).map_err(|error| {
                vec![CompilerDiagnostic::new(
                    source_id,
                    CompilePhase::Module,
                    error.span.unwrap_or_default(),
                    error.to_string(),
                )]
            })?;
            let mut modules: Vec<_> = loader
                .loaded_modules()
                .map(|module| CompiledModule {
                    id: module.id,
                    source_id: self.sources.add_file(&module.path, module.source.clone()),
                    path: module.path.clone(),
                    exports: module.exports.clone(),
                })
                .collect();
            modules.sort_by(|left, right| left.path.cmp(&right.path));
            let mut items = imported_items;
            items.extend(parsed.items);
            (
                SourceFile {
                    items,
                    span: parsed.span,
                },
                modules,
            )
        } else {
            (parsed, Vec::new())
        };

        let mut type_checker = TypeChecker::new();
        type_checker.check(&ast).map_err(|errors| {
            errors
                .into_iter()
                .map(|error| {
                    CompilerDiagnostic::new(
                        source_id,
                        CompilePhase::Type,
                        error.span,
                        error.to_string(),
                    )
                })
                .collect::<Vec<_>>()
        })?;

        let expression_types = type_checker.expression_types().clone();

        let mut borrow_checker = BorrowChecker::new();
        borrow_checker.check_structural(&ast).map_err(|errors| {
            errors
                .into_iter()
                .map(|error| {
                    CompilerDiagnostic::new(
                        source_id,
                        CompilePhase::Borrow,
                        error.span,
                        error.to_string(),
                    )
                    .with_help(error.help)
                })
                .collect::<Vec<_>>()
        })?;

        let mut program = Lowerer::new().lower(&ast).map_err(|errors| {
            errors
                .into_iter()
                .map(|error| {
                    CompilerDiagnostic::new(
                        source_id,
                        CompilePhase::Lower,
                        error.span,
                        error.message,
                    )
                })
                .collect::<Vec<_>>()
        })?;

        // Ownership always runs on unoptimized typed MIR. Strict enforcement is
        // enabled once the lowerer has retired all ambiguous Local operands.
        let ownership = mir::ownership::analyze(&program);
        if !ownership.errors.is_empty() {
            return Err(ownership
                .errors
                .iter()
                .map(|error| {
                    CompilerDiagnostic::new(
                        source_id,
                        CompilePhase::Ownership,
                        Span::default(),
                        error.to_string(),
                    )
                })
                .collect());
        }

        mir::ownership::elaborate_drops(&mut program);
        let ownership = mir::ownership::analyze(&program);
        if !ownership.errors.is_empty() {
            return Err(ownership
                .errors
                .iter()
                .map(|error| {
                    CompilerDiagnostic::new(
                        source_id,
                        CompilePhase::Ownership,
                        Span::default(),
                        error.to_string(),
                    )
                })
                .collect());
        }

        let effects = crate::builtins::infer_effects(&program);
        let backend_support = crate::support::analyze(&program);
        Ok(Compilation {
            source_id,
            ast,
            program,
            ownership,
            effects,
            semantics,
            backend_support,
            expression_types,
            modules,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_map_deduplicates_files() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("main.forma");
        std::fs::write(&path, "f main() -> Int = 0\n").unwrap();

        let mut map = SourceMap::new();
        let first = map.add_file(&path, "first");
        let second = map.add_file(&path, "second");
        assert_eq!(first, second);
        assert_eq!(map.len(), 1);
        assert_eq!(map.source(first), Some("first"));
    }

    #[test]
    fn session_runs_the_complete_pipeline() {
        let mut session = CompilerSession::new();
        let compilation = session
            .compile_source("test.forma", "f main() -> Int = 42\n")
            .unwrap();

        assert!(compilation.program.functions.contains_key("main"));
        assert!(compilation.ownership.is_ready_for_enforcement());
        assert_eq!(
            session.sources().name(compilation.source_id),
            Some("test.forma")
        );
    }

    #[test]
    fn diagnostics_identify_the_failed_phase() {
        let mut session = CompilerSession::new();
        let errors = session
            .compile_source("bad.forma", "f main( -> Int = 0\n")
            .unwrap_err();

        assert!(
            errors
                .iter()
                .all(|error| error.phase == CompilePhase::Parse)
        );
    }

    #[test]
    fn file_compilation_loads_imports() {
        let dir = tempfile::tempdir().unwrap();
        let helper = dir.path().join("helper.forma");
        let main = dir.path().join("main.forma");
        std::fs::write(&helper, "pub f answer() -> Int = 42\n").unwrap();
        std::fs::write(&main, "us helper\nf main() -> Int = answer()\n").unwrap();

        let mut session = CompilerSession::new();
        let compilation = session.compile_file(&main).unwrap();
        assert!(compilation.program.functions.contains_key("answer"));
        assert!(compilation.program.functions.contains_key("main"));
        assert_eq!(
            session.sources().len(),
            2,
            "imported source must be indexed"
        );
        assert_eq!(compilation.modules.len(), 1);
        assert_eq!(compilation.modules[0].source_id.index(), 1);
    }

    #[test]
    fn private_module_items_are_not_imported() {
        let dir = tempfile::tempdir().unwrap();
        let helper = dir.path().join("helper.forma");
        let main = dir.path().join("main.forma");
        std::fs::write(
            &helper,
            "pub f visible() -> Int = 1\nf hidden() -> Int = 2\n",
        )
        .unwrap();
        std::fs::write(&main, "us helper\nf main() -> Int = hidden()\n").unwrap();

        let mut session = CompilerSession::new();
        let diagnostics = session.compile_file(&main).unwrap_err();
        assert!(diagnostics.iter().any(|diagnostic| {
            diagnostic.phase == CompilePhase::Type && diagnostic.message.contains("hidden")
        }));
    }

    #[test]
    fn package_compilation_writes_a_deterministic_lockfile() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir(dir.path().join("src")).unwrap();
        std::fs::write(
            dir.path().join("forma.toml"),
            "[package]\nname = \"sample\"\nversion = \"0.1.0\"\n[deps]\n",
        )
        .unwrap();
        let main = dir.path().join("src/main.forma");
        std::fs::write(&main, "f main() -> Int = 0\n").unwrap();

        CompilerSession::new().compile_file(&main).unwrap();
        let first = std::fs::read_to_string(dir.path().join("forma.lock")).unwrap();
        CompilerSession::new().compile_file(&main).unwrap();
        let second = std::fs::read_to_string(dir.path().join("forma.lock")).unwrap();
        assert_eq!(first, second);
        assert!(first.contains("name = \"sample\""));
    }

    #[test]
    fn fixed_array_repeat_does_not_implicitly_clone_affine_values() {
        let source = r#"
f main() -> Int
    value := ["owned"]
    repeated := [value; 2]
    0
"#;
        let diagnostics = CompilerSession::new()
            .compile_source("repeat.forma", source)
            .unwrap_err();
        assert!(diagnostics.iter().any(|diagnostic| {
            diagnostic.phase == CompilePhase::Ownership && diagnostic.message.contains("moved")
        }));
    }

    #[test]
    fn projected_move_rejects_second_read_of_same_field() {
        let source = r#"
s Pair { left: Str, right: Str }

f main() -> Int
    pair := Pair { left: "a", right: "b" }
    first := pair.left
    second := pair.left
    0
"#;
        let mut session = CompilerSession::new();
        let diagnostics = session.compile_source("partial.forma", source).unwrap_err();
        assert!(diagnostics.iter().any(|diagnostic| {
            diagnostic.phase == CompilePhase::Ownership
                && diagnostic.message.contains("partially moved")
        }));
    }

    #[test]
    fn sibling_fields_can_be_moved_independently() {
        let source = r#"
s Pair { left: Str, right: Str }

f main() -> Int
    pair := Pair { left: "a", right: "b" }
    first := pair.left
    second := pair.right
    0
"#;
        let mut session = CompilerSession::new();
        let compilation = session.compile_source("siblings.forma", source).unwrap();
        assert!(compilation.ownership.is_ready_for_enforcement());
    }

    #[test]
    fn assigning_a_moved_field_makes_it_available_again() {
        let source = r#"
s Pair { left: Str, right: Str }

f main() -> Int
    pair := Pair { left: "a", right: "b" }
    first := pair.left
    pair.left := "replacement"
    second := pair.left
    0
"#;
        let mut session = CompilerSession::new();
        let compilation = session.compile_source("reinit.forma", source).unwrap();
        assert!(compilation.ownership.is_ready_for_enforcement());
    }

    #[test]
    fn closure_capture_moves_owned_values() {
        let source = r#"
f main() -> Int
    message := "owned"
    closure := |x: Int| message
    again := message
    0
"#;
        let mut session = CompilerSession::new();
        let diagnostics = session.compile_source("capture.forma", source).unwrap_err();
        assert!(diagnostics.iter().any(|diagnostic| {
            diagnostic.phase == CompilePhase::Ownership
                && diagnostic.message.contains("moved value")
        }));
    }

    #[test]
    fn closure_capture_copies_copy_values() {
        let source = r#"
f main() -> Int
    number := 41
    closure := |x: Int| number + x
    number + 1
"#;
        let mut session = CompilerSession::new();
        let compilation = session
            .compile_source("copy_capture.forma", source)
            .unwrap();
        assert!(compilation.ownership.is_ready_for_enforcement());
    }

    #[test]
    fn validated_named_copy_values_remain_available_after_assignment() {
        let source = r#"
@derive(Copy)
s Point
    x: Int
    y: Int

f main() -> Int
    point := Point { x: 20, y: 22 }
    duplicate := point
    point.x + duplicate.y
"#;
        let mut session = CompilerSession::new();
        let compilation = session.compile_source("named_copy.forma", source).unwrap();
        assert!(compilation.ownership.is_ready_for_enforcement());
        assert!(compilation.program.copy_types.contains("Point"));
    }

    #[test]
    fn compiler_uses_non_lexical_mir_loan_regions() {
        let source = r#"
f main() -> Int
    value := "first"
    borrowed := &value
    observed := *borrowed
    value = "second"
    0
"#;
        let mut session = CompilerSession::new();
        let compilation = session.compile_source("nll.forma", source).unwrap();
        assert!(compilation.ownership.is_ready_for_enforcement());
    }

    #[test]
    fn replacing_an_owned_binding_emits_a_drop() {
        let source = r#"
f main() -> Int
    value := "first"
    value = "second"
    0
"#;
        let mut session = CompilerSession::new();
        let compilation = session.compile_source("replace.forma", source).unwrap();
        let main = &compilation.program.functions["main"];
        let value_local = main
            .locals
            .iter()
            .position(|decl| decl.name.as_deref() == Some("value"))
            .map(|index| crate::mir::Local(index as u32))
            .unwrap();
        assert!(main.blocks.iter().flat_map(|block| &block.stmts).any(
            |statement| matches!(statement.kind, crate::mir::StatementKind::Drop(local) if local == value_local)
        ));
    }

    #[test]
    fn returned_database_result_is_moved_without_cleanup() {
        // FORGE-RUST-GAP: FRG-016
        let source = r#"
f open_database() -> Database!Str
    db_open_memory()

f main() -> Int
    0
"#;
        let mut session = CompilerSession::new();
        let compilation = session
            .compile_source("returned_database.forma", source)
            .unwrap();
        let function = &compilation.program.functions["open_database"];
        assert_eq!(
            function.return_ty,
            crate::types::Ty::Result(
                Box::new(crate::types::Ty::Database),
                Box::new(crate::types::Ty::Str),
            )
        );
        assert!(matches!(
            function.locals.first().map(|local| &local.ty),
            Some(crate::types::Ty::Result(ok, _))
                if matches!(ok.as_ref(), crate::types::Ty::Database)
        ));
        assert!(matches!(
            function.blocks[1].terminator,
            Some(crate::mir::Terminator::Return(Some(
                crate::mir::Operand::Move(_)
            )))
        ));
        assert!(
            !function
                .blocks
                .iter()
                .flat_map(|block| &block.stmts)
                .any(|statement| matches!(statement.kind, crate::mir::StatementKind::Drop(_)))
        );
    }

    #[test]
    fn long_keyword_aliases_compile() {
        let source = r#"
function main() -> Int
    return 0
"#;
        let mut session = CompilerSession::new();
        let compilation = session.compile_source("aliases.forma", source).unwrap();
        assert!(compilation.program.functions.contains_key("main"));
    }
}
