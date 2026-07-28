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
fn test_cli_typeof_returns_semantic_identifier_type() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("types.forma");
    std::fs::write(&path, "f answer() -> Int = 42\n").unwrap();
    let output = Command::new(forma_bin())
        .args(["typeof", "--position", "1:3"])
        .arg(path)
        .output()
        .expect("failed to execute forma");
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("() -> Int"),
        "unexpected typeof output: {stdout}"
    );
    assert!(!stdout.contains("run type checker"));
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
fn test_cli_fmt_write_preserves_comments_and_literal_spelling() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("lossless.forma");
    let source = "# leading comment\nf main() -> Str = r`a  b` # trailing comment";
    std::fs::write(&path, source).unwrap();

    let output = Command::new(forma_bin())
        .args(["fmt", "--write"])
        .arg(&path)
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "fmt failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        std::fs::read_to_string(path).unwrap(),
        format!("{source}\n")
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
fn test_cli_build_borrow_error_json() {
    // Borrow errors are reported before optional LLVM code generation.
    let output = Command::new(forma_bin())
        .args(["--error-format", "json", "build"])
        .arg(fixture("borrow_error.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        !output.status.success(),
        "forma build must reject source-level ownership violations"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"BORROW\""),
        "JSON output should contain BORROW error category, got: {}",
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

#[test]
fn test_cli_explain_human() {
    let output = Command::new(forma_bin())
        .args(["explain"])
        .arg(fixture("with_contracts.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(output.status.success(), "forma explain should exit 0");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Requires:"),
        "explain output should include preconditions, got: {}",
        stdout
    );
    assert!(
        stdout.contains("Guarantees:"),
        "explain output should include postconditions, got: {}",
        stdout
    );
}

#[test]
fn test_cli_explain_json() {
    let output = Command::new(forma_bin())
        .args(["explain", "--format", "json"])
        .arg(fixture("with_contracts.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "forma explain --format json should exit 0"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"functions\""),
        "JSON explain output should contain functions array, got: {}",
        stdout
    );
}

#[test]
fn test_cli_explain_examples() {
    let output = Command::new(forma_bin())
        .args(["explain", "--examples", "--seed", "7"])
        .arg(fixture("with_contracts.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "forma explain --examples should exit 0"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Examples:"),
        "explain examples output should contain Examples section, got: {}",
        stdout
    );
    assert!(
        stdout.contains("[invalid]"),
        "explain examples output should include invalid counterexamples, got: {}",
        stdout
    );
}

#[test]
fn test_cli_explain_examples_count() {
    let output = Command::new(forma_bin())
        .args(["explain", "--examples=2", "--format", "json", "--seed", "7"])
        .arg(fixture("with_contracts.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "forma explain --examples 2 should exit 0"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // 2 valid + up to 2 invalid counterexamples
    let valid_count = stdout.matches("\"kind\": \"valid\"").count();
    assert!(
        valid_count >= 2,
        "expected at least 2 valid examples in JSON output, got: {}",
        stdout
    );
}

#[test]
fn test_cli_explain_missing_import_json() {
    let output = Command::new(forma_bin())
        .args([
            "--error-format",
            "json",
            "explain",
            "--format",
            "json",
            "--examples",
        ])
        .arg(fixture("missing_import.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        !output.status.success(),
        "explain on missing import should exit nonzero"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"MODULE\""),
        "JSON explain error output should contain MODULE code, got: {}",
        stdout
    );
}

#[test]
fn test_cli_verify_report_json() {
    let output = Command::new(forma_bin())
        .args(["verify", "--report", "--format", "json"])
        .arg(fixture("verify_contract_pass.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "verify --report on valid contracts should exit 0"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"summary\""),
        "verify JSON output should contain summary, got: {}",
        stdout
    );
    assert!(stdout.contains("\"status\": \"TESTED\""));
    assert!(stdout.contains("\"seed\""));
    assert!(stdout.contains("\"tested_domain\""));
    assert!(!stdout.contains("PROVED"));
}

#[test]
fn test_cli_verify_exhausts_finite_bool_domain() {
    let dir = tempfile::tempdir().unwrap();
    let source = dir.path().join("finite.forma");
    std::fs::write(
        &source,
        "@post(result == value)\nf identity(value: Bool) -> Bool = value\n",
    )
    .unwrap();
    let output = Command::new(forma_bin())
        .args([
            "verify",
            "--report",
            "--format",
            "json",
            "--level",
            "exhaustive",
        ])
        .arg(&source)
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let report: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    let function = &report["files"][0]["functions"][0];
    assert_eq!(function["status"], "EXHAUSTIVE");
    assert_eq!(function["examples_run"], 2);
    assert!(function["seed"].is_null());
}

#[test]
fn test_cli_formal_level_reports_unknown_without_solver() {
    let output = Command::new(forma_bin())
        .args([
            "verify", "--report", "--format", "json", "--level", "formal",
        ])
        .arg(fixture("verify_contract_pass.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(output.status.success());
    let report: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(report["files"][0]["functions"][0]["status"], "UNKNOWN");
    assert_eq!(report["summary"]["proved"], 0);
}

#[test]
fn test_cli_explain_and_verify_surface_struct_invariants() {
    let dir = tempfile::tempdir().unwrap();
    let source = dir.path().join("invariant.forma");
    std::fs::write(
        &source,
        r#"@inv(balance >= 0, "balance must be non-negative")
s Account
    balance: Int

@post(result >= 0)
f balance(account: Account) -> Int = account.balance
"#,
    )
    .unwrap();

    let explained = Command::new(forma_bin())
        .args(["explain", "--format", "json"])
        .arg(&source)
        .output()
        .expect("failed to explain invariants");
    assert!(
        explained.status.success(),
        "{}",
        String::from_utf8_lossy(&explained.stderr)
    );
    let explanation: serde_json::Value = serde_json::from_slice(&explained.stdout).unwrap();
    assert_eq!(explanation["structs"][0]["name"], "Account");
    assert_eq!(
        explanation["structs"][0]["invariants"][0]["pattern_name"],
        "inv"
    );

    let verified = Command::new(forma_bin())
        .args(["verify", "--report", "--format", "json"])
        .arg(&source)
        .output()
        .expect("failed to report invariant obligations");
    assert!(
        verified.status.success(),
        "{}",
        String::from_utf8_lossy(&verified.stderr)
    );
    let report: serde_json::Value = serde_json::from_slice(&verified.stdout).unwrap();
    assert_eq!(report["files"][0]["invariants"][0]["type_name"], "Account");
    assert_eq!(report["files"][0]["invariants"][0]["invariant_count"], 1);
    assert_eq!(
        report["files"][0]["invariants"][0]["formal_status"],
        "UNKNOWN"
    );
    assert_eq!(report["summary"]["structs_with_invariants"], 1);
    assert_eq!(report["summary"]["invariant_clauses"], 1);
}

#[test]
fn test_cli_verify_report_missing_import_json() {
    let output = Command::new(forma_bin())
        .args(["verify", "--report", "--format", "json"])
        .arg(fixture("missing_import.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        !output.status.success(),
        "verify --report should fail for missing import"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"file_error\""),
        "verify JSON output should include file_error for compile failures, got: {}",
        stdout
    );
}

#[test]
fn test_cli_verify_report_timeout() {
    let output = Command::new(forma_bin())
        .args([
            "verify",
            "--report",
            "--format",
            "json",
            "--examples",
            "1",
            "--timeout",
            "0",
        ])
        .arg(fixture("verify_contract_pass.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        !output.status.success(),
        "verify --timeout 0 should fail with timeout"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("timeout"),
        "verify timeout output should mention timeout, got: {}",
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

// ---------------------------------------------------------------------------
// MIR Optimization tests
// ---------------------------------------------------------------------------

/// Get the path to a .forma test file in tests/forma/.
fn forma_test(name: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path.push("forma");
    path.push(name);
    path
}

#[test]
fn test_cli_preserves_returned_database_resource() {
    // FORGE-RUST-GAP: FRG-016
    let output = Command::new(forma_bin())
        .args(["run", "--allow-write"])
        .arg(forma_test("test_returned_database_resource.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "returned Database handle should remain valid after Result matching: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn test_cli_grants_explicit_capabilities_to_spawned_tasks() {
    // FORGE-RUST-GAP: FRG-017
    let allowed = Command::new(forma_bin())
        .args(["run", "--allow-read"])
        .arg(forma_test("test_spawned_task_capabilities.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        allowed.status.success(),
        "spawned task should inherit the explicitly granted read capability: {}",
        String::from_utf8_lossy(&allowed.stderr)
    );

    let allowed_all = Command::new(forma_bin())
        .args(["run", "--allow-all"])
        .arg(forma_test("test_spawned_task_capabilities.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        allowed_all.status.success(),
        "spawned task should inherit read through --allow-all: {}",
        String::from_utf8_lossy(&allowed_all.stderr)
    );

    let denied = Command::new(forma_bin())
        .arg("run")
        .arg(forma_test("test_spawned_task_capabilities.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(!denied.status.success());
    let stderr = String::from_utf8_lossy(&denied.stderr);
    assert!(stderr.contains("capability 'read' required"), "{stderr}");
    assert!(
        !stderr.contains("field projection on non-struct"),
        "{stderr}"
    );
}

#[test]
fn test_cli_run_with_optimization() {
    // test_optimization.forma should pass with optimization enabled (default)
    let output = Command::new(forma_bin())
        .args(["run", "--allow-all"])
        .arg(forma_test("test_optimization.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "test_optimization.forma should pass with optimization: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("All optimization tests passed"),
        "Expected pass message, got: {}",
        stdout
    );
}

#[test]
fn test_cli_run_without_optimization() {
    // test_optimization.forma should produce identical output with --no-optimize
    let output = Command::new(forma_bin())
        .args(["run", "--allow-all", "--no-optimize"])
        .arg(forma_test("test_optimization.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "test_optimization.forma should pass with --no-optimize: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("All optimization tests passed"),
        "Expected pass message with --no-optimize, got: {}",
        stdout
    );
}

#[test]
fn test_cli_optimization_output_equivalence() {
    // Run with and without optimization, compare stdout
    let opt_output = Command::new(forma_bin())
        .args(["run", "--allow-all"])
        .arg(forma_test("test_optimization.forma"))
        .output()
        .expect("failed to execute forma");

    let noopt_output = Command::new(forma_bin())
        .args(["run", "--allow-all", "--no-optimize"])
        .arg(forma_test("test_optimization.forma"))
        .output()
        .expect("failed to execute forma");

    assert_eq!(
        opt_output.status.code(),
        noopt_output.status.code(),
        "Exit codes should match"
    );
    assert_eq!(
        String::from_utf8_lossy(&opt_output.stdout),
        String::from_utf8_lossy(&noopt_output.stdout),
        "Stdout should be identical with and without optimization"
    );
}

#[test]
fn test_cli_copy_prop_soundness() {
    // Copy propagation soundness: reassignment after copy must not break semantics
    let opt_output = Command::new(forma_bin())
        .args(["run", "--allow-all"])
        .arg(forma_test("test_copy_prop_soundness.forma"))
        .output()
        .expect("failed to execute forma");

    let noopt_output = Command::new(forma_bin())
        .args(["run", "--allow-all", "--no-optimize"])
        .arg(forma_test("test_copy_prop_soundness.forma"))
        .output()
        .expect("failed to execute forma");

    assert!(
        opt_output.status.success(),
        "copy prop soundness should pass with optimization: {}",
        String::from_utf8_lossy(&opt_output.stderr)
    );
    assert!(
        noopt_output.status.success(),
        "copy prop soundness should pass without optimization: {}",
        String::from_utf8_lossy(&noopt_output.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&opt_output.stdout),
        String::from_utf8_lossy(&noopt_output.stdout),
        "Copy prop soundness output should be identical with and without optimization"
    );
}

// ===== Sprint 51: Functional collection + option chaining tests =====

#[test]
fn test_cli_functional_operations() {
    let output = Command::new(forma_bin())
        .args(["run", "--allow-all"])
        .arg(forma_test("test_functional.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "test_functional.forma should pass: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("All functional tests passed"),
        "Expected success message, got: {}",
        stdout
    );
}

#[test]
fn test_cli_option_chaining() {
    let output = Command::new(forma_bin())
        .args(["run", "--allow-all"])
        .arg(forma_test("test_option_chaining.forma"))
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "test_option_chaining.forma should pass: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("All option chaining tests passed"),
        "Expected success message, got: {}",
        stdout
    );
}

#[test]
fn test_cli_grammar_json_uses_structured_keyword_catalog() {
    let output = Command::new(forma_bin())
        .args(["grammar", "--format", "json"])
        .output()
        .expect("failed to execute forma");
    assert!(output.status.success());
    let grammar: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(grammar["version"], "0.2.0");
    let keywords = grammar["rules"]["keywords"].as_array().unwrap();
    assert!(keywords.iter().any(|keyword| {
        keyword["canonical"] == "f"
            && keyword["aliases"]
                .as_array()
                .is_some_and(|aliases| aliases.iter().any(|alias| alias == "function"))
    }));
}

#[test]
fn test_checked_in_grammar_artifacts_cannot_drift() {
    let output = Command::new(forma_bin())
        .args(["grammar", "--check"])
        .output()
        .expect("failed to execute forma");
    assert!(
        output.status.success(),
        "grammar artifacts are stale: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[cfg(feature = "llvm")]
#[test]
fn test_core_program_builds_and_runs_natively() {
    let dir = tempfile::tempdir().unwrap();
    let source = dir.path().join("main.forma");
    let executable = dir.path().join("main-native");
    std::fs::write(&source, "f main() -> Int = 42\n").unwrap();

    let build = Command::new(forma_bin())
        .args(["build", "--output"])
        .arg(&executable)
        .arg(&source)
        .output()
        .expect("failed to execute forma build");
    assert!(
        build.status.success(),
        "native build failed: {}",
        String::from_utf8_lossy(&build.stderr)
    );
    let run = Command::new(executable).output().unwrap();
    assert_eq!(run.status.code(), Some(42));
}

#[cfg(feature = "llvm")]
#[test]
fn test_projected_tuple_program_matches_interpreter_and_native() {
    let dir = tempfile::tempdir().unwrap();
    let source = dir.path().join("projection.forma");
    let executable = dir.path().join("projection-native");
    std::fs::write(
        &source,
        "f main() -> Int\n    pair := (1, 2)\n    pair.1 := 41\n    pair.0 + pair.1\n",
    )
    .unwrap();

    let interpreted = Command::new(forma_bin())
        .args(["run", "--allow-all"])
        .arg(&source)
        .output()
        .unwrap();
    assert_eq!(
        interpreted.status.code(),
        Some(42),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&interpreted.stdout),
        String::from_utf8_lossy(&interpreted.stderr)
    );

    let built = Command::new(forma_bin())
        .args(["build", "--output"])
        .arg(&executable)
        .arg(&source)
        .output()
        .unwrap();
    assert!(
        built.status.success(),
        "{}",
        String::from_utf8_lossy(&built.stderr)
    );
    let native = Command::new(&executable).output().unwrap();
    assert_eq!(native.status.code(), Some(42));
}

#[cfg(feature = "llvm")]
#[test]
fn test_projected_struct_program_matches_interpreter_and_native() {
    let dir = tempfile::tempdir().unwrap();
    let source = dir.path().join("structure.forma");
    let executable = dir.path().join("structure-native");
    std::fs::write(
        &source,
        "s Pair\n    left: Int\n    right: Int\n\nf main() -> Int\n    pair := Pair { left: 1, right: 2 }\n    pair.right := 41\n    pair.left + pair.right\n",
    )
    .unwrap();
    let interpreted = Command::new(forma_bin())
        .args(["run", "--allow-all"])
        .arg(&source)
        .output()
        .unwrap();
    assert_eq!(interpreted.status.code(), Some(42));
    let built = Command::new(forma_bin())
        .args(["build", "--output"])
        .arg(&executable)
        .arg(&source)
        .output()
        .unwrap();
    assert!(
        built.status.success(),
        "{}",
        String::from_utf8_lossy(&built.stderr)
    );
    assert_eq!(Command::new(executable).status().unwrap().code(), Some(42));
}

#[cfg(feature = "llvm")]
#[test]
fn test_core_loop_matches_interpreter_and_native() {
    let dir = tempfile::tempdir().unwrap();
    let source = dir.path().join("loop.forma");
    let executable = dir.path().join("loop-native");
    std::fs::write(
        &source,
        "f main() -> Int\n    index := 0\n    total := 0\n    wh index < 6\n        total := total + index\n        index := index + 1\n    total\n",
    )
    .unwrap();
    let interpreted = Command::new(forma_bin())
        .args(["run", "--allow-all"])
        .arg(&source)
        .output()
        .unwrap();
    assert_eq!(interpreted.status.code(), Some(15));
    let built = Command::new(forma_bin())
        .args(["build", "--output"])
        .arg(&executable)
        .arg(&source)
        .output()
        .unwrap();
    assert!(
        built.status.success(),
        "{}",
        String::from_utf8_lossy(&built.stderr)
    );
    assert_eq!(Command::new(executable).status().unwrap().code(), Some(15));
}

#[cfg(feature = "llvm")]
#[test]
fn test_enum_discriminants_match_interpreter_and_native() {
    let dir = tempfile::tempdir().unwrap();
    let source = dir.path().join("enum.forma");
    let executable = dir.path().join("enum-native");
    std::fs::write(
        &source,
        "e Choice\n    First\n    Second\n\nf main() -> Int\n    choice := Second\n    m choice\n        First -> 1\n        Second -> 42\n",
    )
    .unwrap();

    let interpreted = Command::new(forma_bin())
        .args(["run", "--allow-all"])
        .arg(&source)
        .output()
        .unwrap();
    assert_eq!(
        interpreted.status.code(),
        Some(42),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&interpreted.stdout),
        String::from_utf8_lossy(&interpreted.stderr)
    );
    let built = Command::new(forma_bin())
        .args(["build", "--output"])
        .arg(&executable)
        .arg(&source)
        .output()
        .unwrap();
    assert!(
        built.status.success(),
        "{}",
        String::from_utf8_lossy(&built.stderr)
    );
    let native = Command::new(executable).output().unwrap();
    assert_eq!(native.status.code(), Some(42));
}

#[cfg(feature = "llvm")]
#[test]
fn test_option_payload_matches_interpreter_and_native() {
    let dir = tempfile::tempdir().unwrap();
    let source = dir.path().join("option.forma");
    let executable = dir.path().join("option-native");
    std::fs::write(
        &source,
        "f main() -> Int\n    value := Some(42)\n    m value\n        Some(x) -> x\n        None -> 0\n",
    )
    .unwrap();
    let interpreted = Command::new(forma_bin())
        .args(["run", "--allow-all"])
        .arg(&source)
        .output()
        .unwrap();
    assert_eq!(interpreted.status.code(), Some(42));
    let built = Command::new(forma_bin())
        .args(["build", "--output"])
        .arg(&executable)
        .arg(&source)
        .output()
        .unwrap();
    assert!(
        built.status.success(),
        "{}",
        String::from_utf8_lossy(&built.stderr)
    );
    assert_eq!(Command::new(executable).status().unwrap().code(), Some(42));
}

#[cfg(feature = "llvm")]
#[test]
fn test_fixed_array_projection_matches_interpreter_and_native() {
    let dir = tempfile::tempdir().unwrap();
    let source = dir.path().join("array.forma");
    let executable = dir.path().join("array-native");
    std::fs::write(
        &source,
        "f main() -> Int\n    values := [1; 3]\n    values[1] := 39\n    values[0] + values[1] + values[2]\n",
    )
    .unwrap();
    let interpreted = Command::new(forma_bin())
        .args(["run", "--allow-all"])
        .arg(&source)
        .output()
        .unwrap();
    assert_eq!(interpreted.status.code(), Some(41));
    let built = Command::new(forma_bin())
        .args(["build", "--output"])
        .arg(&executable)
        .arg(&source)
        .output()
        .unwrap();
    assert!(
        built.status.success(),
        "{}",
        String::from_utf8_lossy(&built.stderr)
    );
    assert_eq!(Command::new(executable).status().unwrap().code(), Some(41));
}

#[test]
fn test_guest_exit_is_translated_only_by_cli_host() {
    let dir = tempfile::tempdir().unwrap();
    let source = dir.path().join("exit.forma");
    std::fs::write(&source, "f main()\n    exit(23)\n").unwrap();
    let output = Command::new(forma_bin())
        .args(["run", "--allow-all"])
        .arg(source)
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(23));
}
