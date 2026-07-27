//! Credential-gated Forge PostgreSQL integration coverage.
//! FORGE-RUST-GAP: FRG-002 (black-box host boundary only).

use std::path::PathBuf;
use std::process::Command;

#[test]
fn forge_postgres_round_trip_when_configured() {
    if std::env::var_os("FORMA_TEST_POSTGRES_URL").is_none() {
        eprintln!("skipping: FORMA_TEST_POSTGRES_URL is not configured");
        return;
    }

    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let output = Command::new(env!("CARGO_BIN_EXE_forma"))
        .current_dir(&root)
        .args([
            "run",
            "--allow-write",
            "--allow-network",
            "--allow-env",
            "examples/forge/src/postgres_check.forma",
        ])
        .output()
        .expect("launch Forma PostgreSQL integration check");

    assert!(
        output.status.success(),
        "PostgreSQL integration failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        String::from_utf8_lossy(&output.stdout).contains("PostgreSQL integration valid"),
        "missing PostgreSQL success marker"
    );
}
