//! Module loader for FORMA.
//!
//! This module handles loading and resolving external modules from files.
//! It supports the `us` (use) statement syntax:
//! - `us std.core` -> looks for `std/core.forma`
//! - `us my_module` -> looks for `my_module.forma`

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::lexer::{Scanner, Span};
use crate::parser::{Item, ItemKind, Parser, SourceFile, UseTree, Visibility};

/// Stable identity derived from a module's canonical source path.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u64);

impl ModuleId {
    fn from_path(path: &Path) -> Self {
        // Fixed FNV-1a keeps IDs stable across sessions and Rust releases.
        let mut hash = 0xcbf29ce484222325u64;
        for byte in path.to_string_lossy().as_bytes() {
            hash ^= u64::from(*byte);
            hash = hash.wrapping_mul(0x100000001b3);
        }
        Self(hash)
    }
}

/// Error during module loading.
#[derive(Debug, Clone)]
pub struct ModuleError {
    pub message: String,
    pub path: Option<PathBuf>,
    /// Span of the `us` statement that triggered this error.
    pub span: Option<Span>,
}

impl std::fmt::Display for ModuleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(path) = &self.path {
            write!(f, "{}: {}", path.display(), self.message)
        } else {
            write!(f, "{}", self.message)
        }
    }
}

impl std::error::Error for ModuleError {}

/// Loaded module information.
#[derive(Debug, Clone)]
pub struct LoadedModule {
    pub id: ModuleId,
    pub path: PathBuf,
    pub source: String,
    pub items: Vec<Item>,
    pub exports: Vec<String>,
}

/// Module loader that handles resolving and loading external modules.
pub struct ModuleLoader {
    /// Base directory for resolving relative module paths
    base_dir: PathBuf,
    /// Cache of already loaded modules
    loaded: HashMap<PathBuf, LoadedModule>,
    /// Set of modules currently being loaded (for cycle detection)
    loading: HashSet<PathBuf>,
    dependency_roots: HashMap<String, PathBuf>,
    manifest_error: Option<String>,
}

impl ModuleLoader {
    /// Create a new module loader with the given base directory.
    pub fn new(base_dir: impl Into<PathBuf>) -> Self {
        Self {
            base_dir: base_dir.into(),
            loaded: HashMap::new(),
            loading: HashSet::new(),
            dependency_roots: HashMap::new(),
            manifest_error: None,
        }
    }

    /// Create a loader from a source file path.
    pub fn from_source_file(source_path: &Path) -> Self {
        let base_dir = source_path
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| PathBuf::from("."));
        let mut loader = Self::new(base_dir);
        match crate::package::PackageManifest::discover(source_path) {
            Ok(Some(manifest)) => {
                loader.dependency_roots = manifest
                    .dependencies
                    .into_iter()
                    .map(|(name, path)| (name, manifest.root.join(path)))
                    .collect();
            }
            Ok(None) => {}
            Err(error) => loader.manifest_error = Some(error),
        }
        loader
    }

    pub fn loaded_modules(&self) -> impl Iterator<Item = &LoadedModule> {
        self.loaded.values()
    }

    /// Resolve a module path to a file path.
    ///
    /// Module paths like `std.core` are converted to `std/core.forma`.
    fn resolve_module_path(&self, module_path: &[String]) -> PathBuf {
        let mut path = self.base_dir.clone();
        for segment in module_path {
            path.push(segment);
        }
        path.set_extension("forma");
        path
    }

    /// Extract module paths from a UseTree.
    fn extract_use_paths(tree: &UseTree, current_path: &[String], paths: &mut Vec<Vec<String>>) {
        match tree {
            UseTree::Path(segments, subtree) => {
                let mut new_path = current_path.to_vec();
                for seg in segments {
                    new_path.push(seg.name.clone());
                }
                if let Some(sub) = subtree {
                    Self::extract_use_paths(sub, &new_path, paths);
                } else {
                    paths.push(new_path);
                }
            }
            UseTree::Rename(segments, _) => {
                let mut new_path = current_path.to_vec();
                for seg in segments {
                    new_path.push(seg.name.clone());
                }
                paths.push(new_path);
            }
            UseTree::Group(trees) => {
                for t in trees {
                    Self::extract_use_paths(t, current_path, paths);
                }
            }
            UseTree::Glob => {
                // For glob imports, we'd need the parent path
                if !current_path.is_empty() {
                    paths.push(current_path.to_vec());
                }
            }
        }
    }

    /// Load a single module from a file.
    fn load_module_file(&mut self, path: &Path) -> Result<LoadedModule, ModuleError> {
        // Check if already loaded
        if let Some(module) = self.loaded.get(path) {
            return Ok(module.clone());
        }

        // Check for cycles
        if self.loading.contains(path) {
            return Err(ModuleError {
                message: "circular module dependency detected".to_string(),
                path: Some(path.to_path_buf()),
                span: None,
            });
        }

        // Mark as loading
        self.loading.insert(path.to_path_buf());

        // Do the actual load work; always clean up loading set afterwards
        let result = self.load_module_file_inner(path);
        self.loading.remove(path);
        result
    }

    /// Inner helper for load_module_file — separated so loading set cleanup is guaranteed.
    fn load_module_file_inner(&mut self, path: &Path) -> Result<LoadedModule, ModuleError> {
        // Read the file
        let source = std::fs::read_to_string(path).map_err(|e| ModuleError {
            message: format!("failed to read file: {}", e),
            path: Some(path.to_path_buf()),
            span: None,
        })?;

        // Lex
        let scanner = Scanner::new(&source);
        let (tokens, lex_errors) = scanner.scan_all();
        if !lex_errors.is_empty() {
            return Err(ModuleError {
                message: format!(
                    "lexer errors: {}",
                    lex_errors
                        .iter()
                        .map(|e| e.message.as_str())
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                path: Some(path.to_path_buf()),
                span: None,
            });
        }

        // Parse
        let parser = Parser::new(&tokens);
        let ast = parser.parse().map_err(|errors| ModuleError {
            message: format!(
                "parse error: {}",
                errors
                    .iter()
                    .map(|e| format!("{}", e))
                    .collect::<Vec<_>>()
                    .join("; ")
            ),
            path: Some(path.to_path_buf()),
            span: None,
        })?;

        let canonical_path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        let exports = ast.items.iter().filter_map(exported_item_name).collect();
        let module = LoadedModule {
            id: ModuleId::from_path(&canonical_path),
            path: canonical_path,
            source: source.clone(),
            items: ast.items.clone(),
            exports,
        };

        // Cache the result
        self.loaded.insert(path.to_path_buf(), module.clone());

        Ok(module)
    }

    /// Resolve a module path to a file, trying base_dir, cwd, and std/ directory.
    fn find_module_file(&self, module_path: &[String]) -> Result<PathBuf, ModuleError> {
        self.find_module_file_from(module_path, &self.base_dir)
    }

    /// Resolve relative to the source containing the import. Package/std
    /// fallbacks remain rooted at the compiler session's base directory.
    fn find_module_file_from(
        &self,
        module_path: &[String],
        importer_dir: &Path,
    ) -> Result<PathBuf, ModuleError> {
        if let Some((dependency, remainder)) = module_path.split_first()
            && let Some(root) = self.dependency_roots.get(dependency)
        {
            let mut dependency_path = root.join("src");
            if remainder.is_empty() {
                dependency_path.push("lib");
            } else {
                for segment in remainder {
                    dependency_path.push(segment);
                }
            }
            dependency_path.set_extension("forma");
            if dependency_path.exists() {
                return Ok(dependency_path.canonicalize().unwrap_or(dependency_path));
            }
            return Err(ModuleError {
                message: format!(
                    "module `{}` was not found in path dependency `{dependency}`",
                    module_path.join(".")
                ),
                path: Some(dependency_path),
                span: None,
            });
        }

        let mut file_path = importer_dir.to_path_buf();
        for segment in module_path {
            file_path.push(segment);
        }
        file_path.set_extension("forma");
        if file_path.exists() {
            return Ok(file_path.canonicalize().unwrap_or(file_path));
        }

        // Package-root resolution supports explicitly qualified paths.
        let package_path = self.resolve_module_path(module_path);
        if package_path.exists() {
            return Ok(package_path.canonicalize().unwrap_or(package_path));
        }

        // Try relative to working directory
        let cwd_path = PathBuf::from(".")
            .join(module_path.join("/"))
            .with_extension("forma");
        if cwd_path.exists() {
            return Ok(cwd_path.canonicalize().unwrap_or(cwd_path));
        }

        // Try std/ directory for stdlib modules (std.core -> std/core.forma)
        if module_path.first().map(|s| s.as_str()) == Some("std") {
            let mut std_path = PathBuf::from("std");
            for segment in module_path.iter().skip(1) {
                std_path.push(segment);
            }
            std_path.set_extension("forma");
            if std_path.exists() {
                return Ok(std_path.canonicalize().unwrap_or(std_path));
            }
        }

        // Module not found
        let tried = format!("'{}', '{}'", file_path.display(), package_path.display());
        Err(ModuleError {
            message: format!(
                "module not found: '{}' (tried {})",
                module_path.join("."),
                tried
            ),
            path: None,
            span: None,
        })
    }

    /// Load all modules referenced by use statements in the given AST.
    /// Returns the combined items from all loaded modules, including transitive imports.
    pub fn load_imports(&mut self, ast: &SourceFile) -> Result<Vec<Item>, ModuleError> {
        if let Some(error) = self.manifest_error.take() {
            return Err(ModuleError {
                message: error,
                path: None,
                span: None,
            });
        }
        let mut all_imported_items = Vec::new();

        for item in &ast.items {
            if let ItemKind::Use(use_item) = &item.kind {
                let import_span = item.span;
                let mut paths = Vec::new();
                Self::extract_use_paths(&use_item.tree, &[], &mut paths);

                for module_path in paths {
                    let file_path = self.find_module_file(&module_path).map_err(|mut e| {
                        e.span = Some(import_span);
                        e
                    })?;
                    self.load_module_recursive(&file_path, &mut all_imported_items)
                        .map_err(|mut e| {
                            if e.span.is_none() {
                                e.span = Some(import_span);
                            }
                            e
                        })?;
                }
            }
        }

        let mut names = HashSet::new();
        for item in &all_imported_items {
            if let Some(name) = exported_item_name(item)
                && !names.insert(name.clone())
            {
                return Err(ModuleError {
                    message: format!(
                        "ambiguous import: multiple modules export `{name}`; rename one export until qualified module namespaces are enabled"
                    ),
                    path: None,
                    span: ast
                        .items
                        .iter()
                        .find(|item| matches!(item.kind, ItemKind::Use(_)))
                        .map(|item| item.span),
                });
            }
        }

        Ok(all_imported_items)
    }

    /// Recursively load a module and its transitive imports.
    /// Uses the `loading` set for cycle detection during transitive resolution.
    fn load_module_recursive(
        &mut self,
        path: &Path,
        items: &mut Vec<Item>,
    ) -> Result<(), ModuleError> {
        let path_buf = path.to_path_buf();

        // Cycle detection FIRST: if this path is currently being resolved, it's circular
        if self.loading.contains(&path_buf) {
            return Err(ModuleError {
                message: "circular module dependency detected".to_string(),
                path: Some(path_buf),
                span: None,
            });
        }

        // If already fully loaded and not in a loading cycle, skip
        if self.loaded.contains_key(&path_buf) {
            return Ok(());
        }

        // Mark as loading for cycle detection during transitive resolution
        self.loading.insert(path_buf.clone());

        // Load and parse the file (load_module_file will also mark/unmark loading,
        // but we re-insert above so it stays marked during transitive resolution)
        let module = match self.load_module_file_inner(path) {
            Ok(m) => m,
            Err(e) => {
                self.loading.remove(&path_buf);
                return Err(e);
            }
        };

        // First, recursively resolve any Use items from this module (transitive imports)
        let use_items: Vec<Item> = module
            .items
            .iter()
            .filter(|i| matches!(i.kind, ItemKind::Use(_)))
            .cloned()
            .collect();

        for use_item in &use_items {
            if let ItemKind::Use(use_decl) = &use_item.kind {
                let mut paths = Vec::new();
                Self::extract_use_paths(&use_decl.tree, &[], &mut paths);
                for module_path in paths {
                    let importer_dir = path.parent().unwrap_or(&self.base_dir);
                    let dep_path = match self.find_module_file_from(&module_path, importer_dir) {
                        Ok(p) => p,
                        Err(e) => {
                            self.loading.remove(&path_buf);
                            return Err(e);
                        }
                    };
                    if let Err(e) = self.load_module_recursive(&dep_path, items) {
                        self.loading.remove(&path_buf);
                        return Err(e);
                    }
                }
            }
        }

        // Done resolving — remove from loading
        self.loading.remove(&path_buf);

        // Then expose only the module's public surface. Implementations carry
        // no standalone visibility in the current AST, but must accompany
        // exported types so their trait and inherent methods remain usable.
        for item in module.items {
            if exported_item_name(&item).is_some() || matches!(item.kind, ItemKind::Impl(_)) {
                items.push(item);
            }
        }

        Ok(())
    }

    /// Load a file and all its dependencies, returning a combined AST.
    pub fn load_with_dependencies(
        &mut self,
        source_path: &Path,
    ) -> Result<SourceFile, ModuleError> {
        // Load the main file
        let main_module = self.load_module_file(source_path)?;
        let main_ast = SourceFile {
            items: main_module.items,
            span: Span {
                start: 0,
                end: 0,
                line: 0,
                column: 0,
            },
        };

        // Load imports
        let imported_items = self.load_imports(&main_ast)?;

        // Combine: imports first, then main file items
        let mut combined_items = imported_items;
        combined_items.extend(main_ast.items);

        Ok(SourceFile {
            items: combined_items,
            span: main_ast.span,
        })
    }
}

fn exported_item_name(item: &Item) -> Option<String> {
    let (name, visibility) = match &item.kind {
        ItemKind::Function(value) => (&value.name.name, value.visibility),
        ItemKind::Struct(value) => (&value.name.name, value.visibility),
        ItemKind::Enum(value) => (&value.name.name, value.visibility),
        ItemKind::Trait(value) => (&value.name.name, value.visibility),
        ItemKind::Module(value) => (&value.name.name, value.visibility),
        ItemKind::Const(value) => (&value.name.name, value.visibility),
        ItemKind::TypeAlias(value) => (&value.name.name, value.visibility),
        ItemKind::Impl(_) | ItemKind::Use(_) => return None,
    };
    (visibility == Visibility::Public).then(|| name.clone())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    /// Create a temp directory with forma files for testing
    fn write_temp_file(dir: &Path, name: &str, content: &str) {
        let path = dir.join(name);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).unwrap();
        }
        let mut f = std::fs::File::create(&path).unwrap();
        f.write_all(content.as_bytes()).unwrap();
    }

    #[test]
    fn test_transitive_import() {
        let dir = tempfile::tempdir().unwrap();
        let base = dir.path();

        // b.forma defines a function
        write_temp_file(base, "b.forma", "pub f helper() -> Int = 99\n");

        // a.forma imports b
        write_temp_file(base, "a.forma", "us b\npub f wrapper() -> Int = helper()\n");

        // main.forma imports a (should transitively get b's items)
        write_temp_file(base, "main.forma", "us a\nf main() -> Int = wrapper()\n");

        let main_path = base.join("main.forma");
        let mut loader = ModuleLoader::from_source_file(&main_path);
        let result = loader.load_with_dependencies(&main_path);
        assert!(
            result.is_ok(),
            "transitive import should succeed: {:?}",
            result.err()
        );

        let ast = result.unwrap();
        let names: Vec<String> = ast
            .items
            .iter()
            .filter_map(|item| {
                if let ItemKind::Function(f) = &item.kind {
                    Some(f.name.name.clone())
                } else {
                    None
                }
            })
            .collect();

        // Should contain items from both a and b
        assert!(
            names.contains(&"helper".to_string()),
            "should contain 'helper' from b.forma"
        );
        assert!(
            names.contains(&"wrapper".to_string()),
            "should contain 'wrapper' from a.forma"
        );
        assert!(
            names.contains(&"main".to_string()),
            "should contain 'main' from main.forma"
        );
    }

    #[test]
    fn module_identity_is_stable_and_exports_are_explicit() {
        let dir = tempfile::tempdir().unwrap();
        let base = dir.path();
        write_temp_file(
            base,
            "api.forma",
            "pub f visible() -> Int = 1\nf hidden() -> Int = 2\n",
        );
        let path = base.join("api.forma");
        let mut first = ModuleLoader::new(base);
        let first_module = first.load_module_file(&path).unwrap();
        let mut second = ModuleLoader::new(base);
        let second_module = second.load_module_file(&path).unwrap();
        assert_eq!(first_module.id, second_module.id);
        assert_eq!(first_module.exports, vec!["visible"]);
    }

    #[test]
    fn package_path_dependencies_resolve_deterministically() {
        let directory = tempfile::tempdir().unwrap();
        let app = directory.path().join("app");
        let dependency = directory.path().join("utility");
        std::fs::create_dir_all(app.join("src")).unwrap();
        std::fs::create_dir_all(dependency.join("src")).unwrap();
        write_temp_file(
            &app,
            "forma.toml",
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n[deps]\nutility = { path = \"../utility\" }\n",
        );
        write_temp_file(&dependency, "src/lib.forma", "pub f answer() -> Int = 42\n");
        write_temp_file(
            &app,
            "src/main.forma",
            "us utility\nf main() -> Int = answer()\n",
        );

        let main = app.join("src/main.forma");
        let mut loader = ModuleLoader::from_source_file(&main);
        let imports = loader
            .load_imports(
                &Parser::new(&Scanner::new("us utility\n").scan_all().0)
                    .parse()
                    .unwrap(),
            )
            .unwrap();
        assert!(imports.iter().any(
            |item| matches!(&item.kind, ItemKind::Function(function) if function.name.name == "answer")
        ));
    }

    #[test]
    fn duplicate_unqualified_exports_are_rejected() {
        let directory = tempfile::tempdir().unwrap();
        write_temp_file(
            directory.path(),
            "a.forma",
            "pub f duplicate() -> Int = 1\n",
        );
        write_temp_file(
            directory.path(),
            "b.forma",
            "pub f duplicate() -> Int = 2\n",
        );
        write_temp_file(
            directory.path(),
            "main.forma",
            "us a\nus b\nf main() -> Int = duplicate()\n",
        );
        let main = directory.path().join("main.forma");
        let result = ModuleLoader::from_source_file(&main).load_with_dependencies(&main);
        let error = result.expect_err("ambiguous flattened exports must not be source-order based");
        assert!(
            error
                .message
                .contains("multiple modules export `duplicate`")
        );
    }

    #[test]
    fn transitive_imports_resolve_relative_to_the_importer() {
        let dir = tempfile::tempdir().unwrap();
        let base = dir.path();
        write_temp_file(base, "feature/helper.forma", "pub f helper() -> Int = 7\n");
        write_temp_file(
            base,
            "feature/api.forma",
            "us helper\npub f api() -> Int = helper()\n",
        );
        write_temp_file(
            base,
            "main.forma",
            "us feature.api\nf main() -> Int = api()\n",
        );

        let main_path = base.join("main.forma");
        let mut loader = ModuleLoader::from_source_file(&main_path);
        let ast = loader
            .load_with_dependencies(&main_path)
            .expect("nested dependency should resolve beside its importer");
        assert!(ast.items.iter().any(
            |item| matches!(&item.kind, ItemKind::Function(function) if function.name.name == "helper")
        ));
    }

    #[test]
    fn test_circular_import_detected() {
        let dir = tempfile::tempdir().unwrap();
        let base = dir.path();

        write_temp_file(base, "x.forma", "us y\nf fx() -> Int = 1\n");
        write_temp_file(base, "y.forma", "us x\nf fy() -> Int = 2\n");
        write_temp_file(base, "main.forma", "us x\nf main() -> Int = fx()\n");

        let main_path = base.join("main.forma");
        let mut loader = ModuleLoader::from_source_file(&main_path);
        let result = loader.load_with_dependencies(&main_path);
        assert!(result.is_err(), "circular import should be detected");
        let err = result.unwrap_err();
        assert!(
            err.message.contains("circular"),
            "error should mention circular: {}",
            err.message
        );
    }

    #[test]
    fn test_import_nonexistent_module_has_span() {
        let dir = tempfile::tempdir().unwrap();
        let base = dir.path();

        // main.forma imports a module that doesn't exist
        write_temp_file(base, "main.forma", "us nonexistent\nf main() -> Int = 0\n");

        let main_path = base.join("main.forma");
        let mut loader = ModuleLoader::from_source_file(&main_path);
        let result = loader.load_with_dependencies(&main_path);
        assert!(result.is_err(), "import of nonexistent module should fail");
        let err = result.unwrap_err();
        assert!(
            err.span.is_some(),
            "module error should have a span pointing to the us statement"
        );
    }

    #[test]
    fn test_lex_error_does_not_poison_loading_set() {
        let dir = tempfile::tempdir().unwrap();
        let base = dir.path();

        // Create a file with a lex error (invalid character)
        write_temp_file(base, "bad.forma", "f foo() -> Int = \x01\n");

        let mut loader = ModuleLoader::new(base);
        let bad_path = base.join("bad.forma");

        // First load attempt should fail
        let result = loader.load_module_file(&bad_path);
        assert!(result.is_err());

        // Second attempt should also fail (not get stuck as "loading")
        let result2 = loader.load_module_file(&bad_path);
        assert!(result2.is_err());
        // Should NOT be a "circular dependency" error
        assert!(
            !result2.unwrap_err().message.contains("circular"),
            "lex error should not poison cycle detection"
        );
    }
}
