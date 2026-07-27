use std::process::Command;

// FORGE-RUST-GAP: FRG-002. This is deliberately a black-box host-boundary
// check: Rust supplies child-process isolation and exit/output assertions while
// Forge configuration and infrastructure assertions remain in Forma.
#[test]
fn forge_configuration_and_infrastructure_are_valid_offline() {
    let root = env!("CARGO_MANIFEST_DIR");
    let configuration = Command::new(env!("CARGO_BIN_EXE_forma"))
        .current_dir(root)
        .args([
            "run",
            "--allow-read",
            "--allow-env",
            "examples/forge/src/main.forma",
            "check",
            "examples/forge/forge.settings.toml",
        ])
        .output()
        .expect("validate the Forge configuration");

    assert!(
        configuration.status.success(),
        "Forge configuration failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&configuration.stdout),
        String::from_utf8_lossy(&configuration.stderr)
    );
    let stdout = String::from_utf8(configuration.stdout).expect("Forge output is UTF-8");
    assert!(stdout.contains("Forge configuration valid"), "{stdout}");
    assert!(stdout.contains("SQLite history:"), "{stdout}");

    let infrastructure = Command::new(env!("CARGO_BIN_EXE_forma"))
        .current_dir(root)
        .args([
            "run",
            "--allow-read",
            "--allow-write",
            "examples/forge/src/infrastructure_check.forma",
        ])
        .output()
        .expect("run Forge infrastructure checks");
    assert!(
        infrastructure.status.success(),
        "Forge infrastructure failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&infrastructure.stdout),
        String::from_utf8_lossy(&infrastructure.stderr)
    );
    let stdout = String::from_utf8(infrastructure.stdout).expect("Forge output is UTF-8");
    assert!(
        stdout.contains("Forge infrastructure checks: 22/22"),
        "{stdout}"
    );

    let tool_plugin = Command::new(env!("CARGO_BIN_EXE_forma"))
        .current_dir(root)
        .args([
            "run",
            "--allow-read",
            "--allow-exec",
            "--allow-env",
            "examples/forge/src/tool_plugin_check.forma",
        ])
        .output()
        .expect("run Forge custom tool plugin check");
    assert!(
        tool_plugin.status.success(),
        "Forge tool plugin check failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&tool_plugin.stdout),
        String::from_utf8_lossy(&tool_plugin.stderr)
    );
    let stdout = String::from_utf8(tool_plugin.stdout).expect("Forge output is UTF-8");
    assert!(
        stdout.contains("Forge tool plugin check passed"),
        "{stdout}"
    );

    let contracts = Command::new(env!("CARGO_BIN_EXE_forma"))
        .current_dir(root)
        .args(["run", "examples/forge/src/contract_check.forma"])
        .output()
        .expect("run Forge positive contract fixture");
    assert!(
        contracts.status.success(),
        "Forge positive contract fixture failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&contracts.stdout),
        String::from_utf8_lossy(&contracts.stderr)
    );

    let violation = Command::new(env!("CARGO_BIN_EXE_forma"))
        .current_dir(root)
        .args(["run", "examples/forge/src/contract_violation.forma"])
        .output()
        .expect("run Forge contract violation fixture");
    assert!(
        !violation.status.success(),
        "Forge contract violation unexpectedly succeeded"
    );
    let diagnostics = format!(
        "{}\n{}",
        String::from_utf8_lossy(&violation.stdout),
        String::from_utf8_lossy(&violation.stderr)
    );
    assert!(
        diagnostics.contains("audit sequence must be non-negative"),
        "{diagnostics}"
    );
}
