//! CLI Integration Tests for FORMA compiler
//!
//! Tests the `forma` binary end-to-end using `std::process::Command`.

use std::path::PathBuf;
use std::process::Command;

/// Get the path to the forma binary (debug build).
fn forma_bin() -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("target");
    path.push("debug");
    path.push("forma");
    path
}

/// Get the path to a test fixture file.
fn fixture(name: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path.push("fixtures");
    path.push(name);
    path
}

#[test]
fn test_cli_run_hello() {
    let output = Command::new(forma_bin())
        .args(["run", "--allow-all"])
        .arg(fixture("hello.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "forma run hello.forma should exit 0"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("hello"), "stdout should contain 'hello'");
}

#[test]
fn test_cli_run_syntax_error() {
    let output = Command::new(forma_bin())
        .args(["run"])
        .arg(fixture("syntax_error.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        !output.status.success(),
        "forma run syntax_error.forma should exit nonzero"
    );
}

#[test]
fn test_cli_run_syntax_error_json() {
    let output = Command::new(forma_bin())
        .args(["--error-format", "json", "run"])
        .arg(fixture("syntax_error.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(!output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"error\"") || stdout.contains("\"errors\""),
        "JSON output should contain error key, got: {}",
        stdout
    );
}

#[test]
fn test_cli_check_hello() {
    let output = Command::new(forma_bin())
        .args(["check"])
        .arg(fixture("hello.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "forma check hello.forma should exit 0"
    );
}

#[test]
fn test_cli_check_type_error() {
    let output = Command::new(forma_bin())
        .args(["check"])
        .arg(fixture("type_error.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        !output.status.success(),
        "forma check type_error.forma should exit nonzero"
    );
}

#[test]
fn test_cli_lex_hello() {
    let output = Command::new(forma_bin())
        .args(["lex"])
        .arg(fixture("hello.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "forma lex hello.forma should exit 0"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Lex output should contain token names
    assert!(
        stdout.contains("Fn") || stdout.contains("fn") || stdout.contains("Ident"),
        "lex output should contain token names, got: {}",
        stdout
    );
}

#[test]
fn test_cli_parse_hello() {
    let output = Command::new(forma_bin())
        .args(["parse"])
        .arg(fixture("hello.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "forma parse hello.forma should exit 0"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Parse output should contain AST-related terms
    assert!(
        stdout.contains("main") || stdout.contains("Function") || stdout.contains("item"),
        "parse output should contain AST terms, got: {}",
        stdout
    );
}

#[test]
fn test_cli_fmt_hello() {
    let output = Command::new(forma_bin())
        .args(["fmt"])
        .arg(fixture("hello.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "forma fmt hello.forma should exit 0"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("main"),
        "fmt output should contain formatted code with 'main'"
    );
}

#[test]
fn test_cli_fmt_json_error() {
    let output = Command::new(forma_bin())
        .args(["--error-format", "json", "fmt"])
        .arg(fixture("syntax_error.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        !output.status.success(),
        "forma fmt syntax_error.forma should exit nonzero"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"error\"") || stdout.contains("\"errors\""),
        "JSON fmt error should contain error key, got: {}",
        stdout
    );
}

#[test]
fn test_cli_run_env_denied() {
    let output = Command::new(forma_bin())
        .args(["run"])
        .arg(fixture("env_usage.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        !output.status.success(),
        "forma run env_usage.forma without --allow-env should exit nonzero"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("capability") || stderr.contains("Capability"),
        "error should mention capability, got: {}",
        stderr
    );
}

#[test]
fn test_cli_run_env_allowed() {
    let output = Command::new(forma_bin())
        .args(["run", "--allow-env"])
        .arg(fixture("env_usage.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "forma run --allow-env env_usage.forma should exit 0"
    );
}

#[test]
fn test_cli_run_allow_all() {
    let output = Command::new(forma_bin())
        .args(["run", "--allow-all"])
        .arg(fixture("env_usage.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "forma run --allow-all env_usage.forma should exit 0"
    );
}

#[test]
fn test_cli_run_no_check_contracts() {
    let output = Command::new(forma_bin())
        .args(["run", "--no-check-contracts"])
        .arg(fixture("contract_fail.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "forma run --no-check-contracts contract_fail.forma should exit 0 (contracts skipped)"
    );
}

#[test]
fn test_cli_run_contract_violation() {
    let output = Command::new(forma_bin())
        .args(["run"])
        .arg(fixture("contract_fail.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        !output.status.success(),
        "forma run contract_fail.forma should exit nonzero (contract violation)"
    );
}

#[test]
fn test_cli_check_missing_import() {
    let output = Command::new(forma_bin())
        .args(["check"])
        .arg(fixture("missing_import.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        !output.status.success(),
        "forma check missing_import.forma should exit nonzero"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let combined = format!("{}{}", stdout, stderr);
    assert!(
        combined.contains("module") || combined.contains("Module"),
        "error should mention module, got stdout: {}, stderr: {}",
        stdout,
        stderr
    );
}

#[test]
fn test_cli_check_missing_import_json() {
    let output = Command::new(forma_bin())
        .args(["--error-format", "json", "check"])
        .arg(fixture("missing_import.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        !output.status.success(),
        "forma --error-format json check missing_import.forma should exit nonzero"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"MODULE\""),
        "JSON output should contain MODULE error category, got: {}",
        stdout
    );
    assert!(
        stdout.contains("\"errors\""),
        "JSON output should contain errors key, got: {}",
        stdout
    );
}

#[test]
fn test_cli_build_missing_import_json() {
    // Module error happens before LLVM codegen, so this test works regardless of llvm feature
    let output = Command::new(forma_bin())
        .args(["--error-format", "json", "build"])
        .arg(fixture("missing_import.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        !output.status.success(),
        "forma --error-format json build missing_import.forma should exit nonzero"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"MODULE\""),
        "JSON output should contain MODULE error category, got: {}",
        stdout
    );
    assert!(
        stdout.contains("\"errors\""),
        "JSON output should contain errors key, got: {}",
        stdout
    );
}

#[test]
fn test_cli_run_missing_import_json() {
    let output = Command::new(forma_bin())
        .args(["--error-format", "json", "run", "--allow-all"])
        .arg(fixture("missing_import.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        !output.status.success(),
        "forma --error-format json run --allow-all missing_import.forma should exit nonzero"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"MODULE\""),
        "JSON output should contain MODULE error category, got: {}",
        stdout
    );
    assert!(
        stdout.contains("\"errors\""),
        "JSON output should contain errors key, got: {}",
        stdout
    );
}

// ---- JSON failure matrix tests (Sprint 45) ----
// Verifies that every (command, error-class) pair emits valid JSON with correct structure.

/// Helper: assert JSON error contract for a given command + fixture
fn assert_json_error(cmd_args: &[&str], fixture_name: &str, expected_code: &str) {
    let mut args = vec!["--error-format", "json"];
    args.extend_from_slice(cmd_args);

    let output = Command::new(forma_bin())
        .args(&args)
        .arg(fixture(fixture_name))
        .output()
        .expect("failed to execute forma");

    assert!(
        !output.status.success(),
        "{:?} {} should exit nonzero",
        cmd_args,
        fixture_name
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"success\":false") || stdout.contains("\"success\": false"),
        "{:?} {} JSON should contain success:false, got: {}",
        cmd_args,
        fixture_name,
        stdout
    );
    assert!(
        stdout.contains("\"errors\""),
        "{:?} {} JSON should contain errors key, got: {}",
        cmd_args,
        fixture_name,
        stdout
    );
    assert!(
        stdout.contains(&format!("\"{}\"", expected_code)),
        "{:?} {} JSON should contain code \"{}\", got: {}",
        cmd_args,
        fixture_name,
        expected_code,
        stdout
    );
}

// --- run command matrix ---

#[test]
fn test_json_matrix_run_lex_error() {
    assert_json_error(&["run"], "lex_error.forma", "LEX");
}

#[test]
fn test_json_matrix_run_parse_error() {
    assert_json_error(&["run"], "syntax_error.forma", "PARSE");
}

#[test]
fn test_json_matrix_run_module_error() {
    assert_json_error(&["run", "--allow-all"], "missing_import.forma", "MODULE");
}

#[test]
fn test_json_matrix_run_type_error() {
    assert_json_error(&["run"], "type_error.forma", "TYPE");
}

// --- check command matrix ---

#[test]
fn test_json_matrix_check_lex_error() {
    assert_json_error(&["check"], "lex_error.forma", "LEX");
}

#[test]
fn test_json_matrix_check_parse_error() {
    assert_json_error(&["check"], "syntax_error.forma", "PARSE");
}

#[test]
fn test_json_matrix_check_module_error() {
    assert_json_error(&["check"], "missing_import.forma", "MODULE");
}

#[test]
fn test_json_matrix_check_type_error() {
    assert_json_error(&["check"], "type_error.forma", "TYPE");
}

// --- build command matrix ---

#[test]
fn test_json_matrix_build_lex_error() {
    assert_json_error(&["build"], "lex_error.forma", "LEX");
}

#[test]
fn test_json_matrix_build_parse_error() {
    assert_json_error(&["build"], "syntax_error.forma", "PARSE");
}

#[test]
fn test_json_matrix_build_module_error() {
    assert_json_error(&["build"], "missing_import.forma", "MODULE");
}

#[test]
fn test_json_matrix_build_type_error() {
    assert_json_error(&["build"], "type_error.forma", "TYPE");
}

// ---- check --partial import-error contract tests (Sprint 45) ----

#[test]
fn test_cli_check_partial_missing_import() {
    // check --partial with a missing import should still fail (module errors are fatal)
    let output = Command::new(forma_bin())
        .args(["check", "--partial"])
        .arg(fixture("missing_import.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        !output.status.success(),
        "check --partial missing_import.forma should exit nonzero"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let combined = format!("{}{}", stdout, stderr);
    assert!(
        combined.contains("module") || combined.contains("Module"),
        "check --partial should report module error, got stdout: {}, stderr: {}",
        stdout,
        stderr
    );
}

#[test]
fn test_cli_check_partial_missing_import_json() {
    // check --partial with JSON + missing import should emit MODULE error in standard format
    let output = Command::new(forma_bin())
        .args(["--error-format", "json", "check", "--partial"])
        .arg(fixture("missing_import.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        !output.status.success(),
        "check --partial --error-format json missing_import.forma should exit nonzero"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"MODULE\""),
        "JSON should contain MODULE error code, got: {}",
        stdout
    );
    assert!(
        stdout.contains("\"errors\""),
        "JSON should contain errors key, got: {}",
        stdout
    );
}

#[test]
fn test_cli_check_partial_type_error_json() {
    // check --partial with JSON + type error should use partial format (valid/holes/items)
    let output = Command::new(forma_bin())
        .args(["--error-format", "json", "check", "--partial"])
        .arg(fixture("type_error.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        !output.status.success(),
        "check --partial type_error.forma should exit nonzero"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"valid\""),
        "partial JSON should contain 'valid' key, got: {}",
        stdout
    );
    assert!(
        stdout.contains("\"items\""),
        "partial JSON should contain 'items' key, got: {}",
        stdout
    );
}

#[test]
fn test_cli_check_partial_success_json() {
    // check --partial with JSON on valid code should return valid:true
    let output = Command::new(forma_bin())
        .args(["--error-format", "json", "check", "--partial"])
        .arg(fixture("hello.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "check --partial hello.forma should exit 0"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"valid\":true") || stdout.contains("\"valid\": true"),
        "partial JSON should contain valid:true, got: {}",
        stdout
    );
}
