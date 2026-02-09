# FORMA Showcase Examples

A collection of canonical programming examples demonstrating FORMA's features and capabilities. These examples are based on [Rosetta Code](https://rosettacode.org/wiki/Category:Programming_Tasks), the [Computer Language Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/index.html), and [PLB2](https://github.com/attractivechaos/plb2).

## Examples

### Basic (Tier 1)
| # | Example | Description | Key Concepts |
|---|---------|-------------|--------------|
| 01 | Hello World | The canonical first program | Output, strings |
| 02 | FizzBuzz | Classic interview problem | Loops, conditionals, modulo |
| 03 | 99 Bottles | Loop exercise with lyrics | Loops, f-strings, functions |

### Mathematical (Tier 2)
| # | Example | Description | Key Concepts |
|---|---------|-------------|--------------|
| 04 | Factorial | n! computation | Recursion, tail recursion, iteration |
| 05 | Fibonacci | Classic sequence | Recursion vs iteration, O(n) vs O(2^n) |
| 06 | Prime Numbers | Sieve of Eratosthenes | Arrays, nested loops, optimization |
| 07 | GCD/LCM | Euclidean algorithm | Recursion, mathematical algorithms |

### Classic Algorithms (Tier 3)
| # | Example | Description | Key Concepts |
|---|---------|-------------|--------------|
| 08 | Tower of Hanoi | Recursive puzzle | Recursion, problem decomposition |
| 09 | N-Queens | Backtracking puzzle | Backtracking, constraint satisfaction |
| 10 | Quicksort | Divide-and-conquer sort | Recursion, array mutation, partitioning |
| 11 | Binary Search | O(log n) search | Iteration, divide-and-conquer |

### Data Structures (Tier 4)
| # | Example | Description | Key Concepts |
|---|---------|-------------|--------------|
| 12 | Linked List | Index-based linked list | Structs, references via indices |
| 13 | Binary Tree | Binary search tree | Tree traversal, recursion |

### Advanced (Tier 5)
| # | Example | Description | Key Concepts |
|---|---------|-------------|--------------|
| 14 | Sudoku Solver | Constraint satisfaction | 2D arrays, backtracking |
| 15 | Matrix Multiplication | Computational benchmark | Nested loops, O(n³) algorithm |
| 16 | Game of Life | Cellular automaton | 2D simulation, neighbor counting |

### Error Handling (Tier 6)
| # | Example | Description | Key Concepts |
|---|---------|-------------|--------------|
| 17a | Error Handling | Option, Result, safe patterns | Option/Result, `?`, `??`, match guards, builtins |

### Verification UX (Tier 7)
| # | File | Description | Key Concepts |
|---|------|-------------|--------------|
| 17 | `17_verified_sort.forma` | Sorting with full behavioral contracts | `@pre`, `@sorted`, `@permutation`, trust report |
| 18 | `18_verified_stack.forma` | Stack operations with pre/post state checks | `old(...)`, tuple postconditions |
| 19 | `19_verified_math.forma` | Numeric and collection invariants | named patterns, quantifiers |
| 20 | `20_verification_demo.forma` | End-to-end explain/verify workflow | PASS/WARN visibility, CI-friendly output |

## Running Examples

```bash
# Run individual examples
cargo run -- run examples/showcase/01_hello_world.forma
cargo run -- run examples/showcase/09_nqueens.forma

# Run all examples
for f in examples/showcase/*.forma; do
    echo "=== Running $f ==="
    cargo run --quiet -- run "$f"
    echo ""
done

# Inspect contracts in human-readable form
cargo run -- explain --examples examples/showcase/17_verified_sort.forma

# Generate trust report for showcase directory
cargo run -- verify --report examples/showcase/
```

## Verification Script

```bash
#!/bin/bash
# verify_showcase.sh - Run all showcase examples and check for errors

cd forma
PASS=0
FAIL=0

for f in examples/showcase/*.forma; do
    name=$(basename "$f")
    echo -n "Testing $name... "

    if cargo run --quiet -- run "$f" > /tmp/output.txt 2>&1; then
        echo "PASS"
        ((PASS++))
    else
        echo "FAIL"
        ((FAIL++))
        cat /tmp/output.txt
    fi
done

echo ""
echo "Results: $PASS passed, $FAIL failed"
```

## Feature Coverage

These examples collectively demonstrate:

- **Control Flow**: if/else, while loops, match expressions, match guards, early return
- **Functions**: Recursion, tail recursion, multiple return values (tuples)
- **Data Types**: Int, String, Bool, arrays, tuples, Option, Result
- **Error Handling**: Option/Result types, `?` propagation, `??` coalescing, `is_some`/`is_ok`/`unwrap`/`unwrap_or`/`expect`
- **String Handling**: f-strings, concatenation, interpolation
- **Array Operations**: Indexing, mutation, parallel arrays, safe access (`vec_get`)
- **Algorithms**: Sorting, searching, backtracking, constraint satisfaction
- **Mathematical**: GCD, LCM, primes, factorials, Fibonacci
- **Verification UX**: Contract explanation, deterministic examples, PASS/WARN/SKIP/FAIL reporting

## Performance Notes

- **Fibonacci**: Naive recursive is O(2^n), iterative is O(n)
- **N-Queens**: Exponential backtracking, but pruning makes it practical
- **Quicksort**: O(n log n) average, O(n²) worst case
- **Binary Search**: O(log n)
- **Matrix Multiplication**: O(n³) - standard algorithm

## Contributing

To add a new example:
1. Create `XX_name.forma` with next available number
2. Include header comment explaining the algorithm
3. Add entry to this README
4. Ensure it compiles and runs with `cargo run -- run`
5. If contract-heavy, validate `cargo run -- explain --examples` and `cargo run -- verify --report`
