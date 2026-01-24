//! Module loader for ARIA.
//!
//! This module handles loading and resolving external modules from files.
//! It supports the `us` (use) statement syntax:
//! - `us stdlib.core` -> looks for `stdlib/core.aria`
//! - `us my_module` -> looks for `my_module.aria`

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::lexer::{Scanner, Span};
use crate::parser::{Parser, Item, ItemKind, SourceFile, UseTree};

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
    /// Module paths like `stdlib.core` are converted to `stdlib/core.aria`.
    fn resolve_module_path(&self, module_path: &[String]) -> PathBuf {
        let mut path = self.base_dir.clone();
        for segment in module_path {
            path.push(segment);
        }
        path.set_extension("aria");
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
                    lex_errors.iter().map(|e| e.message.as_str()).collect::<Vec<_>>().join(", ")
                ),
                path: Some(path.to_path_buf()),
            });
        }

        // Parse
        let parser = Parser::new(&tokens);
        let ast = parser.parse().map_err(|e| ModuleError {
            message: format!("parse error: {}", e),
            path: Some(path.to_path_buf()),
        })?;

        // Done loading this module
        self.loading.remove(path);

        let module = LoadedModule {
            path: path.to_path_buf(),
            items: ast.items.clone(),
        };

        // Cache the result
        self.loaded.insert(path.to_path_buf(), LoadedModule {
            path: path.to_path_buf(),
            items: ast.items,
        });

        Ok(module)
    }

    /// Load all modules referenced by use statements in the given AST.
    /// Returns the combined items from all loaded modules.
    pub fn load_imports(&mut self, ast: &SourceFile) -> Result<Vec<Item>, ModuleError> {
        let mut all_imported_items = Vec::new();

        for item in &ast.items {
            if let ItemKind::Use(use_item) = &item.kind {
                let mut paths = Vec::new();
                Self::extract_use_paths(&use_item.tree, &[], &mut paths);

                for module_path in paths {
                    // Try different path resolutions
                    let file_path = self.resolve_module_path(&module_path);

                    // Try the resolved path first
                    if file_path.exists() {
                        let module = self.load_module_file(&file_path)?;
                        // Add all items from the module
                        for item in module.items {
                            // Skip use statements from imported modules to avoid re-importing
                            if !matches!(item.kind, ItemKind::Use(_)) {
                                all_imported_items.push(item);
                            }
                        }
                    } else {
                        // Try relative to working directory
                        let cwd_path = PathBuf::from(".").join(
                            module_path.join("/")
                        ).with_extension("aria");

                        if cwd_path.exists() {
                            let module = self.load_module_file(&cwd_path)?;
                            for item in module.items {
                                if !matches!(item.kind, ItemKind::Use(_)) {
                                    all_imported_items.push(item);
                                }
                            }
                        }
                        // If neither path exists, we could either error or ignore
                        // For now, we'll silently ignore missing modules
                    }
                }
            }
        }

        Ok(all_imported_items)
    }

    /// Load a file and all its dependencies, returning a combined AST.
    pub fn load_with_dependencies(&mut self, source_path: &Path) -> Result<SourceFile, ModuleError> {
        // Load the main file
        let main_module = self.load_module_file(source_path)?;
        let main_ast = SourceFile {
            items: main_module.items,
            span: Span { start: 0, end: 0, line: 0, column: 0 },
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
