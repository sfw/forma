use std::path::{Path, PathBuf};

use forma::CompilerSession;

fn forma_files(directory: &Path) -> Vec<PathBuf> {
    // FORGE-RUST-GAP: FRG-003. Package examples use nested `src/` trees, so
    // repository corpus discovery must follow package roots without changing
    // the separate contract of legacy, non-package showcase directories.
    fn visit(directory: &Path, files: &mut Vec<PathBuf>) {
        for entry in std::fs::read_dir(directory).unwrap().filter_map(Result::ok) {
            let path = entry.path();
            if path.is_dir() {
                visit(&path, files);
            } else if path
                .extension()
                .is_some_and(|extension| extension == "forma")
            {
                files.push(path);
            }
        }
    }

    let mut files = Vec::new();
    for entry in std::fs::read_dir(directory).unwrap().filter_map(Result::ok) {
        let path = entry.path();
        if path.is_file()
            && path
                .extension()
                .is_some_and(|extension| extension == "forma")
        {
            files.push(path);
        } else if path.is_dir() && path.join("forma.toml").is_file() {
            visit(&path, &mut files);
        }
    }
    files.sort();
    files
}

#[test]
fn repository_program_corpus_passes_the_shared_pipeline() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let mut files = forma_files(&root.join("examples"));
    files.extend(forma_files(&root.join("tests/forma")));
    assert!(!files.is_empty());

    let mut failures = Vec::new();
    for file in files {
        if let Err(diagnostics) = CompilerSession::new().compile_file(&file) {
            failures.push(format!(
                "{}:\n{}",
                file.strip_prefix(&root).unwrap_or(&file).display(),
                diagnostics
                    .iter()
                    .map(|diagnostic| format!("  {:?}: {}", diagnostic.phase, diagnostic.message))
                    .collect::<Vec<_>>()
                    .join("\n")
            ));
        }
    }

    assert!(
        failures.is_empty(),
        "repository Forma corpus failed:\n{}",
        failures.join("\n")
    );
}
