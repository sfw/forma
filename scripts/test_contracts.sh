#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT_DIR"

FORMA_BIN="${FORMA_BIN:-./target/release/forma}"

echo "Contract test suite"
echo "==================="

echo "Building release binary..."
cargo build --release

echo "Running targeted Rust tests..."
cargo test contract -- --nocapture
cargo test explain -- --nocapture
cargo test verify -- --nocapture

echo "Running contract-focused CLI checks..."
"$FORMA_BIN" explain tests/fixtures/with_contracts.forma --examples --seed 42 --max-examples 3 > /dev/null
"$FORMA_BIN" verify tests/fixtures/verify_contract_pass.forma --report --format json --examples 10 --seed 42 > /dev/null
"$FORMA_BIN" --error-format json verify tests/fixtures/missing_import.forma --report --format json > /tmp/forma_contract_missing_import.json || true

if ! rg -q "\"file_error\"" /tmp/forma_contract_missing_import.json; then
  echo "Expected missing-import verify output to include file_error"
  exit 1
fi

echo "Running FORMA contract integration files..."
"$FORMA_BIN" run --allow-all tests/forma/test_contracts.forma > /dev/null
"$FORMA_BIN" run --allow-all tests/forma/test_contract_expressions.forma > /dev/null
"$FORMA_BIN" run --allow-all tests/forma/test_patterns.forma > /dev/null
"$FORMA_BIN" run --allow-all tests/forma/test_old_expr.forma > /dev/null
"$FORMA_BIN" run --allow-all tests/forma/test_quantifiers.forma > /dev/null
"$FORMA_BIN" run --allow-all tests/forma/test_contracts_optional.forma > /dev/null
"$FORMA_BIN" run --allow-all --no-check-contracts tests/forma/test_contract_expressions.forma > /dev/null

echo "Running intentional error-path contract test..."
if "$FORMA_BIN" run --allow-all tests/forma/test_contract_errors.forma > /tmp/forma_contract_errors.out 2>&1; then
  echo "Expected test_contract_errors.forma to fail, but it succeeded"
  exit 1
fi
if ! rg -q "Contract violation|precondition" /tmp/forma_contract_errors.out; then
  echo "Expected contract error output to mention contract violation"
  cat /tmp/forma_contract_errors.out
  exit 1
fi

echo "Contract test suite passed."
