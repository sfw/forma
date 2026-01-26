#!/bin/bash
set -e

echo "=========================================="
echo "FORMA v1.0 Final Verification"
echo "=========================================="
echo ""

cd "$(dirname "$0")/.."

echo "=== 1. Cargo Build ==="
cargo build --release
echo "Build successful"
echo ""

echo "=== 2. Rust Unit Tests ==="
cargo test --quiet
echo "All Rust tests pass"
echo ""

echo "=== 3. FORMA Integration Tests ==="
FORMA="cargo run --quiet --"
PASSED=0
FAILED=0
for f in tests/forma/*.forma; do
    if $FORMA run "$f" > /dev/null 2>&1; then
        echo "  $f"
        PASSED=$((PASSED + 1))
    else
        echo "  FAIL: $f"
        FAILED=$((FAILED + 1))
    fi
done
echo "Integration tests: $PASSED passed, $FAILED failed"
echo ""

echo "=== 4. Examples Compile Check ==="
for f in examples/*.forma; do
    if $FORMA check "$f" > /dev/null 2>&1; then
        echo "  $f compiles"
    else
        echo "  FAIL: $f"
        FAILED=$((FAILED + 1))
    fi
done
echo ""

echo "=== 5. Stdlib Modules ==="
for f in std/*.forma; do
    if $FORMA check "$f" > /dev/null 2>&1; then
        echo "  $f"
    else
        echo "  FAIL: $f"
        FAILED=$((FAILED + 1))
    fi
done
echo ""

echo "=== 6. Import System ==="
cat > /tmp/test_imports.forma << 'EOF'
us std.core
us std.json
us std.vec

f main() -> Int
    print("Imports work!")
    0
EOF
if $FORMA run /tmp/test_imports.forma > /dev/null 2>&1; then
    echo "  Import system working"
else
    echo "  FAIL: Import system"
    FAILED=$((FAILED + 1))
fi
echo ""

echo "=== 7. JSON Complete Test ==="
if $FORMA run tests/forma/test_json_complete.forma > /dev/null 2>&1; then
    echo "  JSON fully functional (24/24 tests)"
else
    echo "  FAIL: JSON tests"
    FAILED=$((FAILED + 1))
fi
echo ""

echo "=== 8. Async Test ==="
if $FORMA run tests/forma/test_async_spawn.forma > /dev/null 2>&1; then
    echo "  Async/spawn working"
else
    echo "  FAIL: Async tests"
    FAILED=$((FAILED + 1))
fi
echo ""

echo "=== 9. Literal Pattern Matching ==="
cat > /tmp/test_patterns.forma << 'EOF'
f main() -> Int
    s := "hello"
    result := m s
        "hello" -> true
        _ -> false
    if result then 0 else 1
EOF
if $FORMA run /tmp/test_patterns.forma > /dev/null 2>&1; then
    echo "  String pattern matching working"
else
    echo "  FAIL: String pattern matching"
    FAILED=$((FAILED + 1))
fi
echo ""

echo "=== 10. Grammar Export ==="
if $FORMA grammar > /dev/null 2>&1; then
    echo "  Grammar export working"
else
    echo "  FAIL: Grammar export"
    FAILED=$((FAILED + 1))
fi
echo ""

echo "=== 11. Formatter ==="
if $FORMA fmt examples/hello.forma > /dev/null 2>&1; then
    echo "  Formatter working"
else
    echo "  FAIL: Formatter"
    FAILED=$((FAILED + 1))
fi
echo ""

echo "=========================================="
if [ $FAILED -eq 0 ]; then
    echo "ALL CHECKS PASSED - Ready for v1.0!"
else
    echo "$FAILED checks failed - NOT ready for release"
    exit 1
fi
echo "=========================================="
