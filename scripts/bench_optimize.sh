#!/usr/bin/env bash
# Benchmark MIR optimization pass
# Runs a compute-heavy .forma program with and without optimization
# and reports wall-clock time for each.

set -euo pipefail

FORMA="./target/release/forma"
BENCH_FILE="$(mktemp)"
trap 'rm -f "$BENCH_FILE"' EXIT

# Create a compute-heavy benchmark program
cat > "$BENCH_FILE" << 'FORMA'
# Fibonacci benchmark (tree-recursive, exercises many temps and branches)
f fib(n: Int) -> Int
    if n <= 1 then n
    else fib(n - 1) + fib(n - 2)

# Sum of fibonacci numbers exercises many function calls
f sum_fibs(n: Int) -> Int
    total := 0
    idx := 0
    wh idx <= n
        total := total + fib(idx)
        idx := idx + 1
    total

f main() -> Int
    result := sum_fibs(25)
    print("Result: " + str(result))
    0
FORMA

if [ ! -f "$FORMA" ]; then
    echo "Error: $FORMA not found. Run 'cargo build --release' first."
    exit 1
fi

echo "=== MIR Optimization Benchmark ==="
echo ""

echo "--- With optimization (default) ---"
time_opt=$( { time "$FORMA" run --allow-all "$BENCH_FILE" > /dev/null 2>&1; } 2>&1 )
echo "$time_opt"
echo ""

echo "--- Without optimization (--no-optimize) ---"
time_noopt=$( { time "$FORMA" run --allow-all --no-optimize "$BENCH_FILE" > /dev/null 2>&1; } 2>&1 )
echo "$time_noopt"
echo ""

echo "=== Done ==="
echo "(Compare 'real' times above to measure optimization impact)"
