//! Module loader for FORMA.
//!
//! This module handles loading and resolving external modules from files.
//! It supports the `us` (use) statement syntax:
//! - `us std.core` -> looks for `std/core.forma`
//! - `us my_module` -> looks for `my_module.forma`

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::lexer::{Scanner, Span};
use crate::parser::{Item, ItemKind, Parser, SourceFile, UseTree};

/// Error during module loading.
#[derive(Debug, Clone)]
pub struct ModuleError {
    pub message: String,
    pub path: Option<PathBuf>,
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
#[derive(Debug)]
pub struct LoadedModule {
    pub path: PathBuf,
    pub items: Vec<Item>,
}

/// Module loader that handles resolving and loading external modules.
pub struct ModuleLoader {
    /// Base directory for resolving relative module paths
    base_dir: PathBuf,
    /// Cache of already loaded modules
    loaded: HashMap<PathBuf, LoadedModule>,
    /// Set of modules currently being loaded (for cycle detection)
    loading: HashSet<PathBuf>,
}

impl ModuleLoader {
    /// Create a new module loader with the given base directory.
    pub fn new(base_dir: impl Into<PathBuf>) -> Self {
        Self {
            base_dir: base_dir.into(),
            loaded: HashMap::new(),
            loading: HashSet::new(),
        }
    }

    /// Create a loader from a source file path.
    pub fn from_source_file(source_path: &Path) -> Self {
        let base_dir = source_path
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| PathBuf::from("."));
        Self::new(base_dir)
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
            return Ok(LoadedModule {
                path: module.path.clone(),
                items: module.items.clone(),
            });
        }

        // Check for cycles
        if self.loading.contains(path) {
            return Err(ModuleError {
                message: "circular module dependency detected".to_string(),
                path: Some(path.to_path_buf()),
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
        })?;

        let module = LoadedModule {
            path: path.to_path_buf(),
            items: ast.items.clone(),
        };

        // Cache the result
        self.loaded.insert(
            path.to_path_buf(),
            LoadedModule {
                path: path.to_path_buf(),
                items: ast.items,
            },
        );

        Ok(module)
    }

    /// Resolve a module path to a file, trying base_dir, cwd, and std/ directory.
    fn find_module_file(&self, module_path: &[String]) -> Result<PathBuf, ModuleError> {
        // Try resolved path from base_dir first
        let file_path = self.resolve_module_path(module_path);
        if file_path.exists() {
            return Ok(file_path);
        }

        // Try relative to working directory
        let cwd_path = PathBuf::from(".")
            .join(module_path.join("/"))
            .with_extension("forma");
        if cwd_path.exists() {
            return Ok(cwd_path);
        }

        // Try std/ directory for stdlib modules (std.core -> std/core.forma)
        if module_path.first().map(|s| s.as_str()) == Some("std") {
            let mut std_path = PathBuf::from("std");
            for segment in module_path.iter().skip(1) {
                std_path.push(segment);
            }
            std_path.set_extension("forma");
            if std_path.exists() {
                return Ok(std_path);
            }
        }

        // Module not found
        let tried = format!("'{}'", file_path.display());
        Err(ModuleError {
            message: format!(
                "module not found: '{}' (tried {})",
                module_path.join("."),
                tried
            ),
            path: None,
        })
    }

    /// Load all modules referenced by use statements in the given AST.
    /// Returns the combined items from all loaded modules, including transitive imports.
    pub fn load_imports(&mut self, ast: &SourceFile) -> Result<Vec<Item>, ModuleError> {
        let mut all_imported_items = Vec::new();

        for item in &ast.items {
            if let ItemKind::Use(use_item) = &item.kind {
                let mut paths = Vec::new();
                Self::extract_use_paths(&use_item.tree, &[], &mut paths);

                for module_path in paths {
                    let file_path = self.find_module_file(&module_path)?;
                    self.load_module_recursive(&file_path, &mut all_imported_items)?;
                }
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
                    let dep_path = match self.find_module_file(&module_path) {
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

        // Then add this module's non-Use items
        for item in module.items {
            if !matches!(item.kind, ItemKind::Use(_)) {
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
        write_temp_file(base, "b.forma", "f helper() -> Int = 99\n");

        // a.forma imports b
        write_temp_file(base, "a.forma", "us b\nf wrapper() -> Int = helper()\n");

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
