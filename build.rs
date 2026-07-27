use std::path::Path;

fn main() {
    // Homebrew's llvm-config may report `-lzstd` without its keg-only search
    // path. Publishing an existing standard path keeps LLVM feature builds
    // reproducible without affecting non-Homebrew hosts.
    for path in ["/opt/homebrew/opt/zstd/lib", "/usr/local/opt/zstd/lib"] {
        if Path::new(path).is_dir() {
            println!("cargo:rustc-link-search=native={path}");
        }
    }
}
