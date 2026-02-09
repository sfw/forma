#!/bin/bash
# Verify all showcase examples compile and run

cd "$(dirname "$0")/../.."

echo "FORMA Showcase Verification"
echo "==========================="
echo ""

PASS=0
FAIL=0
TOTAL=0

for f in examples/showcase/*.forma; do
    name=$(basename "$f" .forma)
    ((TOTAL++))
    printf "%-25s " "$name"

    # Run and capture only stdout, ignore stderr (Rust warnings)
    # Check exit code for actual failures
    if cargo run --quiet -- run "$f" 2>/dev/null > /tmp/forma_out_$$.txt; then
        echo "✓ PASS"
        ((PASS++))
    else
        # Capture full output including stderr for error diagnosis
        cargo run --quiet -- run "$f" > /tmp/forma_full_$$.txt 2>&1
        echo "✗ FAIL"
        ((FAIL++))
        echo "  Output:"
        # Filter out Rust warnings, show FORMA errors
        grep -v "^warning:" /tmp/forma_full_$$.txt | grep -v "^ *-->" | grep -v "^ *|" | head -10 | sed 's/^/    /'
    fi
done

echo ""
echo "Checking explain --examples output for showcase examples..."
for f in examples/showcase/*.forma; do
    name=$(basename "$f" .forma)
    printf "explain %-17s " "$name"
    if cargo run --quiet -- explain --examples --max-examples 1 "$f" > /tmp/forma_explain_$$.txt 2>/dev/null; then
        echo "✓ PASS"
    else
        echo "✗ FAIL"
        cargo run --quiet -- explain --examples --max-examples 1 "$f" > /tmp/forma_explain_full_$$.txt 2>&1
        sed 's/^/    /' /tmp/forma_explain_full_$$.txt | head -10
        ((FAIL++))
    fi
done

echo ""
echo "Generating verify report for showcase examples..."
if cargo run --quiet -- verify examples/showcase --report --format json > /tmp/forma_verify_showcase_$$.json 2>/tmp/forma_verify_showcase_$$.err; then
    echo "verify report ✓ PASS"
else
    echo "verify report ✗ FAIL"
    sed 's/^/    /' /tmp/forma_verify_showcase_$$.err | head -20
    ((FAIL++))
fi

echo ""
echo "==========================="
echo "Results: $PASS/$TOTAL passed"

if [ $FAIL -gt 0 ]; then
    echo "FAILURES: $FAIL"
    exit 1
fi

echo "All examples verified successfully!"
