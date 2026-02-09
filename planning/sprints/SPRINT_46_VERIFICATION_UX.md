# Sprint 46: Verification UX — Trust Through Transparency

**Goal:** Make FORMA contracts human-readable so developers can trust AI-generated code without reading implementations.

**Estimated Effort:** 6-8 days

**Key Deliverables:**
- Enhanced contract expressions (`arr[i]`, `old()`, `forall`, `exists`)
- `forma explain` command with English translation
- `forma verify --report` for trust summaries
- Named contract patterns (`@nonempty`, `@sorted`, etc.)
- 4 showcase examples demonstrating verification UX
- Comprehensive test suite

---

## Context

FORMA already has runtime contract enforcement (`@pre`/`@post`) from Sprint 13. However, contracts are still *code* — developers must parse predicate logic to understand what a function promises. This sprint adds tooling that translates contracts into human-friendly formats, making it easy to review and trust AI-generated code.

**Core insight:** Contracts are a communication protocol between AI and human. The AI generates contracts alongside code; the human reviews contracts (not implementation) to establish trust.

---

## Strategic Value

1. **Trust without reading code** — Developers review English specs, not implementations
2. **AI self-correction** — Structured verification output helps AI iterate on failures
3. **Differentiation** — "AI-generated code with built-in trust verification" is a unique pitch
4. **Foundation for static verification** — These features work with future SMT integration

---

## Design Principle: Contracts Are Optional

**Contracts are 100% opt-in.** FORMA code works perfectly without any contracts:

```forma
# This is valid FORMA — no contracts required
f add(a: Int, b: Int) -> Int
    a + b

f main() -> Int
    add(2, 3)
```

Contracts enhance trust but never block functionality:

| Scenario | Behavior |
|----------|----------|
| No contracts | Code compiles and runs normally |
| With `@pre`/`@post` | Runtime checking (can disable with `--no-check-contracts`) |
| With `forma explain` | Shows "no contracts" for uncontracted functions |
| With `forma verify` | WARN status for functions without contracts, not FAIL |

**Why optional matters:**
- Zero barrier to adoption — use FORMA without learning contracts
- Incremental trust — add contracts to critical functions first
- AI flexibility — AI can generate contracts when appropriate, omit when trivial
- Performance — disable contract checking in production if desired

**Gradual adoption path:**
1. Write code without contracts → works fine
2. Add `@pre`/`@post` to critical functions → runtime checking
3. Use `forma explain` → understand what's specified
4. Use `forma verify --report` → see coverage gaps
5. Add more contracts where trust matters → iterate

---

## Feature Overview

| Feature | Purpose | Effort |
|---------|---------|--------|
| 46.1 Enhanced contract expressions | Support array indexing, `old()`, `forall`, `exists`, `=>` | 2 days |
| 46.2 `forma explain` command | Contract → English translation | 1.5 days |
| 46.3 Auto-generated examples | Concrete illustrations of contracts | 1.5 days |
| 46.4 Verification report | Trust summary for project | 1 day |
| 46.5 Named contract patterns | Readable shorthand (`@nonempty`) | 1 day |
| 46.6 Testing regiment | Unit, integration, CLI tests | 0.5 days |
| 46.7 Showcase examples | 4 polished demo programs | 0.5 days |

### New Contract Syntax Summary

This sprint adds the following syntax to contract expressions:

| Syntax | Example | Meaning |
|--------|---------|---------|
| Array indexing | `arr[i]` | Access element at index |
| Tuple field | `t.0` | Access tuple element |
| `old(expr)` | `old(x)` | Pre-call value (postconditions only) |
| `forall` | `forall x in arr: x > 0` | Universal quantifier |
| `exists` | `exists x in arr: x == target` | Existential quantifier |
| `x in arr` | `target in items` | Membership test |
| `a => b` | `n == 0 => result == 1` | Implication (if a then b) |

**Note:** `forall`, `exists`, `old`, and `=>` are contextual — they only have special meaning inside `@pre`/`@post` contracts, not in regular code.

---

## Parser/Lexer Delta Checklist

This sprint adds new syntax that requires careful parser/lexer updates. Complete this checklist before implementation.

### New Tokens

| Token | Lexeme | Context | Notes |
|-------|--------|---------|-------|
| `FORALL` | `forall` | Contracts only | Contextual keyword |
| `EXISTS` | `exists` | Contracts only | Contextual keyword |
| `OLD` | `old` | Contracts only | Contextual keyword |
| `IN` | `in` | Contracts (membership) | Already exists for `for` loops |
| `IMPLIES` | `=>` | Contracts only | Already exists for match arms |

### Contextual Keyword Strategy

`forall`, `exists`, and `old` are **contextual keywords** — they're only reserved inside contract expressions, not in regular code:

```forma
# Valid: 'old' as variable name in regular code
f example()
    old := 5      # OK - 'old' is an identifier here
    forall := 10  # OK - 'forall' is an identifier here

# Inside contracts: keywords
@post(old(x) > 0)        # 'old' is keyword
@pre(forall i in items: i > 0)  # 'forall' is keyword
```

**Implementation:**
1. Add `ParserContext` enum: `{ Normal, Contract }`
2. In contract parsing mode, recognize `forall`/`exists`/`old` as keywords
3. In normal mode, treat them as identifiers
4. Use lookahead to distinguish `old` variable vs `old(expr)` call

### Precedence Rules

| Operator/Construct | Precedence | Associativity | Notes |
|--------------------|------------|---------------|-------|
| `forall`/`exists` ... `:` pred | Lowest | N/A | Binds entire predicate |
| `=>` (implication) | Lower than `||` | Right | `a => b => c` = `a => (b => c)` |
| `in` (membership) | Same as `==` | Left | `x in arr == true` |
| `:` (quantifier body) | Very low | N/A | Everything after `:` is predicate |

### Grammar Additions

```ebnf
(* Contract-specific expression extensions *)
ContractExpr = QuantifierExpr | ImplicationExpr | Expr ;

QuantifierExpr = ("forall" | "exists") Identifier "in" Expr ":" ContractExpr ;

ImplicationExpr = Expr "=>" ContractExpr ;  (* Right-associative *)

MembershipExpr = Expr "in" Expr ;  (* Already supported, ensure contract context works *)

OldExpr = "old" "(" Expr ")" ;  (* Only valid in postconditions *)
```

### Backward Compatibility Tests

Before implementation, verify these still work:

```forma
# 'old', 'forall', 'exists' as identifiers (regression tests)
f test_contextual_keywords()
    old := 5
    forall := [1, 2, 3]
    exists := true
    old + len(forall)  # Should work

# 'in' in for loops (existing behavior)
f test_in_keyword()
    for x in [1, 2, 3]
        print(x)

# '=>' in match arms (existing behavior)
f test_arrow()
    m x? : Int? := Some(5)
    result := ma x
        Some(v) => v * 2
        None => 0
```

### AST Node Additions

```rust
// In ast.rs - new ExprKind variants

/// Quantifier expression: forall x in iter: predicate
Forall {
    var: Ident,
    iterable: Box<Expr>,
    predicate: Box<Expr>,
    span: Span,
},

/// Quantifier expression: exists x in iter: predicate
Exists {
    var: Ident,
    iterable: Box<Expr>,
    predicate: Box<Expr>,
    span: Span,
},

/// Old expression: old(expr) - only valid in postconditions
Old {
    expr: Box<Expr>,
    /// Assigned during MIR lowering for stable storage
    id: Option<OldExprId>,
    span: Span,
},

/// Implication: a => b (right-associative)
Implies(Box<Expr>, Box<Expr>),

/// Membership test: x in collection
In(Box<Expr>, Box<Expr>),
```

### Parser Test Matrix

| Test Case | Input | Expected AST/Error |
|-----------|-------|-------------------|
| Simple forall | `forall x in arr: x > 0` | `Forall { var: x, iter: arr, pred: x > 0 }` |
| Nested forall | `forall x in a: forall y in b: x < y` | Nested Forall nodes |
| Forall with range | `forall i in 0..n: arr[i] >= 0` | Range as iterable |
| Exists | `exists x in arr: x == target` | `Exists { ... }` |
| Old simple | `old(x)` | `Old { expr: x }` |
| Old nested | `old(arr[0])` | `Old { expr: Index(arr, 0) }` |
| Old in precondition | `@pre(old(x) > 0)` | **ERROR**: old() only in postcondition |
| Implication | `a => b` | `Implies(a, b)` |
| Chained implication | `a => b => c` | `Implies(a, Implies(b, c))` |
| In membership | `x in arr` | `In(x, arr)` |
| Contextual old | `old := 5` | `Assign(old, 5)` - identifier, not keyword |

---

## Task 46.1: Enhanced Contract Expressions

**Priority:** P0 (Foundation for other tasks)

**Files:**
- `src/mir/interp.rs` — `eval_contract_expr()`
- `src/parser/parser.rs` — contract parsing
- `src/parser/ast.rs` — new AST nodes (see checklist above)
- `src/lexer/scanner.rs` — contextual keyword handling

### 46.1.1 Array/List Indexing

**Current:** `arr[0]` not supported in contracts
**After:** Full index expression support

```forma
@pre(items[0] > 0)           # First element positive
@post(result == items[idx])  # Returns element at index
```

**Implementation:**
Add `ExprKind::Index` handling to `eval_contract_expr()`:
```rust
ExprKind::Index(base, index) => {
    let base_val = self.eval_contract_expr(base)?;
    let index_val = self.eval_contract_expr(index)?;
    match (&base_val, &index_val) {
        (Value::Array(arr), Value::Int(i)) => {
            arr.get(*i as usize).cloned().ok_or_else(|| InterpError {
                message: format!("index {} out of bounds", i),
            })
        }
        (Value::Str(s), Value::Int(i)) => {
            s.chars().nth(*i as usize)
                .map(Value::Char)
                .ok_or_else(|| InterpError {
                    message: format!("string index {} out of bounds", i),
                })
        }
        _ => Err(InterpError {
            message: format!("cannot index {:?} with {:?}", base_val, index_val),
        }),
    }
}
```

### 46.1.2 Tuple Field Access

**Current:** `t.0` not supported
**After:** Tuple indexing works

```forma
@post(result.0 >= 0 && result.1 >= 0)  # Both tuple elements non-negative
```

**Implementation:**
Add `ExprKind::TupleField` handling:
```rust
ExprKind::TupleField(base, idx) => {
    let base_val = self.eval_contract_expr(base)?;
    match base_val {
        Value::Tuple(fields) => {
            fields.get(*idx).cloned().ok_or_else(|| InterpError {
                message: format!("tuple index {} out of bounds", idx),
            })
        }
        _ => Err(InterpError {
            message: format!("cannot access tuple field on {:?}", base_val),
        }),
    }
}
```

### 46.1.3 `old(expr)` for Postconditions

**Purpose:** Reference pre-call values in postconditions

```forma
@post(result == old(x) + old(y))  # Result is sum of original inputs
@post(items.len() == old(items).len() + 1)  # List grew by one
```

**Implementation approach:**

1. **Parser:** Recognize `old(expr)` as a special form in contract context
2. **MIR Lowering:** Assign stable IDs to `old()` expressions during lowering
3. **Interpreter:**
   - Before function call: evaluate and save `old()` expressions by ID
   - Store in frame as `old_values: HashMap<OldExprId, Value>`
   - During postcondition check: resolve `old(expr)` via its ID

**Data structures:**
```rust
/// Unique identifier for an old() expression (assigned during lowering)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OldExprId(pub u32);

/// Counter for generating unique old expression IDs
static OLD_EXPR_COUNTER: AtomicU32 = AtomicU32::new(0);

pub fn fresh_old_expr_id() -> OldExprId {
    OldExprId(OLD_EXPR_COUNTER.fetch_add(1, Ordering::SeqCst))
}

// In Frame
old_values: HashMap<OldExprId, Value>,  // Stable ID -> saved value

// In MirContract
pub struct OldExprBinding {
    pub id: OldExprId,
    pub expr: Box<Expr>,  // The expression inside old()
}

// In MirContract
old_expressions: Vec<OldExprBinding>,  // Expressions that need pre-saving
```

**Why stable IDs instead of string keys:**
- String keys break on formatting differences (`old(x)` vs `old( x )`)
- String keys can't distinguish structurally identical expressions in different contexts
- IDs assigned at lowering time are stable and unambiguous
- Performance: integer hash is faster than string hash

**Evaluation flow:**
1. During MIR lowering: extract all `old(expr)` subexpressions, assign each a unique `OldExprId`
2. Before call: evaluate each `old_expressions[i].expr`, store as `old_values[id] = value`
3. During postcondition eval: when encountering `old(...)`, lookup by its assigned ID

### 46.1.4 Bounded Quantifiers (`forall`, `exists`)

**Purpose:** Express properties over collections

```forma
@pre(forall i in 0..items.len(): items[i] >= 0)   # All non-negative
@post(exists i in 0..result.len(): result[i] == target)  # Contains target
@post(forall x in result: x > 0)  # All elements positive
```

**Syntax options:**

Option A (range-based):
```forma
forall i in 0..n: predicate
exists i in 0..n: predicate
```

Option B (collection-based):
```forma
forall x in collection: predicate
exists x in collection: predicate
```

**Recommendation:** Support both. Range-based for indices, collection-based for elements.

**Implementation:**

1. **Parser:** Add `ExprKind::Forall` and `ExprKind::Exists`:
```rust
// In ast.rs
Forall(Ident, Box<Expr>, Box<Expr>),  // var, iterable, predicate
Exists(Ident, Box<Expr>, Box<Expr>),
```

2. **Contract evaluator:**
```rust
ExprKind::Forall(var, iterable, predicate) => {
    let iter_val = self.eval_contract_expr(iterable)?;
    let items = self.value_to_iter(&iter_val)?;

    for item in items {
        // Bind var to item in scope
        self.push_contract_binding(&var.name, item);
        let result = self.eval_contract_expr(predicate)?;
        self.pop_contract_binding(&var.name);

        match result {
            Value::Bool(false) => return Ok(Value::Bool(false)),
            Value::Bool(true) => continue,
            _ => return Err(InterpError {
                message: "forall predicate must be Bool".into(),
            }),
        }
    }
    Ok(Value::Bool(true))
}
```

3. **Helper for iteration:**
```rust
fn value_to_iter(&self, val: &Value) -> Result<Vec<Value>, InterpError> {
    match val {
        Value::Array(arr) => Ok(arr.clone()),
        Value::Range(start, end) => {
            Ok((start..end).map(|i| Value::Int(i as i64)).collect())
        }
        _ => Err(InterpError {
            message: format!("cannot iterate over {:?}", val),
        }),
    }
}
```

### 46.1.5 `contains` / `in` Operator

**Purpose:** Check membership without iteration

```forma
@post(result in items)           # Result is from input
@post(target in result)          # Output contains target
@pre(!forbidden in allowed)      # Forbidden not in allowed
```

**Implementation:**
Add as binary operator or method:
```rust
// As method
ExprKind::MethodCall if method == "contains" => {
    match (&receiver_val, &arg_vals[0]) {
        (Value::Array(arr), needle) => {
            Ok(Value::Bool(arr.iter().any(|x| self.values_equal(x, needle))))
        }
        (Value::Str(s), Value::Char(c)) => {
            Ok(Value::Bool(s.contains(*c)))
        }
        (Value::Str(s), Value::Str(sub)) => {
            Ok(Value::Bool(s.contains(sub.as_str())))
        }
        _ => Err(...)
    }
}
```

### Acceptance Criteria (46.1)

1. `arr[i]` works in contracts with bounds checking
2. `t.0` works for tuple field access
3. `old(expr)` captures pre-call values in postconditions
4. `forall x in collection: predicate` evaluates correctly
5. `exists x in collection: predicate` evaluates correctly
6. `forall i in 0..n: predicate` works with ranges
7. `x.contains(y)` and `y in x` work for arrays/strings
8. Clear error messages when contract expressions fail
9. All new features covered by unit tests

---

## Task 46.2: `forma explain` Command

**Priority:** P0

**Files:**
- `src/main.rs` — `explain` subcommand + explanation rendering helpers

### Purpose

Translate contracts into plain English:

```bash
$ forma explain src/sort.forma

┌─────────────────────────────────────────────────────────────────┐
│ sort(items: [Int]) -> [Int]                                     │
├─────────────────────────────────────────────────────────────────┤
│ Requires:                                                       │
│   (none)                                                        │
│                                                                 │
│ Guarantees:                                                     │
│   • Output has same length as input                             │
│   • Output contains all elements from input                     │
│   • Output is sorted in ascending order                         │
└─────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│ binary_search(items: [Int], target: Int) -> Int?                │
├─────────────────────────────────────────────────────────────────┤
│ Requires:                                                       │
│   • items must be sorted in ascending order                     │
│                                                                 │
│ Guarantees:                                                     │
│   • If found: returns index where items[index] == target        │
│   • If not found: returns None                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Implementation

**1. Expression-to-English translator:**

```rust
// src/main.rs (explain helpers)

pub fn explain_expr(expr: &Expr) -> String {
    match &expr.kind {
        // Comparisons
        ExprKind::Binary(left, BinOp::Gt, right) => {
            format!("{} is greater than {}", explain_expr(left), explain_expr(right))
        }
        ExprKind::Binary(left, BinOp::Ge, right) => {
            format!("{} is at least {}", explain_expr(left), explain_expr(right))
        }
        ExprKind::Binary(left, BinOp::Lt, right) => {
            format!("{} is less than {}", explain_expr(left), explain_expr(right))
        }
        ExprKind::Binary(left, BinOp::Eq, right) => {
            format!("{} equals {}", explain_expr(left), explain_expr(right))
        }
        ExprKind::Binary(left, BinOp::Ne, right) => {
            format!("{} is not equal to {}", explain_expr(left), explain_expr(right))
        }

        // Logical
        ExprKind::Binary(left, BinOp::And, right) => {
            format!("{}, and {}", explain_expr(left), explain_expr(right))
        }
        ExprKind::Binary(left, BinOp::Or, right) => {
            format!("{}, or {}", explain_expr(left), explain_expr(right))
        }
        ExprKind::Unary(UnaryOp::Not, inner) => {
            format!("it is not the case that {}", explain_expr(inner))
        }

        // Method calls with special handling
        ExprKind::MethodCall(recv, method, _) if method.name == "len" => {
            format!("the length of {}", explain_expr(recv))
        }
        ExprKind::MethodCall(recv, method, _) if method.name == "is_empty" => {
            format!("{} is empty", explain_expr(recv))
        }

        // Quantifiers
        ExprKind::Forall(var, iter, pred) => {
            format!("for every {} in {}: {}", var.name, explain_expr(iter), explain_expr(pred))
        }
        ExprKind::Exists(var, iter, pred) => {
            format!("there exists {} in {} such that {}", var.name, explain_expr(iter), explain_expr(pred))
        }

        // Special patterns
        ExprKind::Binary(
            box Expr { kind: ExprKind::MethodCall(arr, len, _), .. },
            BinOp::Gt,
            box Expr { kind: ExprKind::Literal(Literal { kind: LiteralKind::Int(0), .. }), .. }
        ) if len.name == "len" => {
            format!("{} is not empty", explain_expr(arr))
        }

        // Identifiers
        ExprKind::Ident(id) if id.name == "result" => "the result".to_string(),
        ExprKind::Ident(id) => id.name.clone(),

        // Literals
        ExprKind::Literal(lit) => format_literal(lit),

        // Fallback: show the expression
        _ => format!("{:?}", expr),  // TODO: pretty-print
    }
}
```

**2. Pattern recognition for common idioms:**

| Pattern | English |
|---------|---------|
| `x > 0` | "x is positive" |
| `x >= 0` | "x is non-negative" |
| `x.len() > 0` | "x is not empty" |
| `x != 0` | "x is non-zero" |
| `result == x + y` | "result equals the sum of x and y" |
| `forall i in 0..n: arr[i] >= 0` | "all elements of arr are non-negative" |
| `result.len() == old(x).len()` | "result has same length as original x" |

**3. CLI integration:**

```rust
// In main.rs
#[derive(Subcommand)]
enum Commands {
    // ... existing commands ...

    /// Explain contracts in human-readable English
    Explain {
        /// Input file
        file: PathBuf,

        /// Specific function to explain (optional)
        #[arg(long)]
        function: Option<String>,

        /// Output format
        #[arg(long, value_enum, default_value = "human")]
        format: ExplainFormat,

        /// Include auto-generated input/output examples (optional count)
        #[arg(long, num_args = 0..=1, default_missing_value = "3", require_equals = true)]
        examples: Option<usize>,

        /// Number of examples per function (alias for --examples=N)
        #[arg(long)]
        max_examples: Option<usize>,

        /// RNG seed for deterministic example generation (used with --examples)
        #[arg(long)]
        seed: Option<u64>,
    },
}

#[derive(Clone, Copy, ValueEnum)]
enum ExplainFormat {
    Human,  // Pretty-printed boxes
    Json,   // For IDE/tooling
    Markdown,  // For documentation
}
```

**4. JSON output for IDE/LSP:**

```json
{
  "functions": [
    {
      "name": "sort",
      "signature": "sort(items: [Int]) -> [Int]",
      "preconditions": [],
      "postconditions": [
        {
          "expression": "result.len() == items.len()",
          "english": "output has same length as input"
        },
        {
          "expression": "forall i in 0..result.len()-1: result[i] <= result[i+1]",
          "english": "output is sorted in ascending order"
        }
      ]
    }
  ]
}
```

### Acceptance Criteria (46.2)

1. `forma explain file.forma` produces human-readable contract summaries
2. Common patterns translated to natural English (not just predicate restatement)
3. `--format json` produces structured output for tooling
4. `--function name` filters to specific function
5. Handles functions with no contracts gracefully
6. Graceful fallback for complex expressions (show expression if can't translate)

---

## Task 46.3: Auto-Generated Examples

**Priority:** P1

**Files:**
- `src/main.rs` — extend `explain` command + example generation helpers
- `src/module/loader.rs` — import resolution reused via `ModuleLoader`

### Purpose

Show concrete input/output examples that illustrate contracts:

```bash
$ forma explain src/math.forma --examples

┌─────────────────────────────────────────────────────────────────┐
│ abs(x: Int) -> Int                                              │
├─────────────────────────────────────────────────────────────────┤
│ Guarantees: result is non-negative                              │
├─────────────────────────────────────────────────────────────────┤
│ Examples:                                                       │
│   abs(-5)  → 5  ✓                                               │
│   abs(0)   → 0  ✓                                               │
│   abs(42)  → 42 ✓                                               │
└─────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│ divide(a: Int, b: Int) -> Int                                   │
├─────────────────────────────────────────────────────────────────┤
│ Requires: b is non-zero                                         │
├─────────────────────────────────────────────────────────────────┤
│ Valid examples:                                                 │
│   divide(10, 2)  → 5  ✓                                         │
│   divide(7, 3)   → 2  ✓                                         │
│   divide(-8, 4)  → -2 ✓                                         │
│                                                                 │
│ Would violate precondition:                                     │
│   divide(5, 0)   → ERROR: b must be non-zero                    │
└─────────────────────────────────────────────────────────────────┘
```

### Implementation

**0. Import resolution contract (must match Sprint 43 behavior):**

- `explain --examples` MUST resolve imports via `ModuleLoader::from_source_file(...)` + `load_imports(...)` before parsing/lowering.
- Missing imports MUST emit structured errors in both human and JSON formats (no silent success).
- Add regression coverage using `tests/fixtures/missing_import.forma`.

**1. Input generation strategies:**

```rust
pub struct ExampleGenerator {
    rng: StdRng,
    max_attempts: usize,
}

impl ExampleGenerator {
    /// Generate inputs that satisfy preconditions
    pub fn generate_valid_inputs(
        &mut self,
        func: &Function,
        count: usize,
    ) -> Vec<Vec<Value>> {
        let mut valid = Vec::new();
        let mut attempts = 0;

        while valid.len() < count && attempts < self.max_attempts {
            let inputs = self.random_inputs_for_params(&func.params);
            if self.satisfies_preconditions(func, &inputs) {
                valid.push(inputs);
            }
            attempts += 1;
        }

        valid
    }

    /// Generate inputs that violate preconditions (for counterexamples)
    pub fn generate_invalid_inputs(
        &mut self,
        func: &Function,
        count: usize,
    ) -> Vec<(Vec<Value>, String)> {  // inputs + which precondition violated
        // Try to violate each precondition
        let mut invalid = Vec::new();

        for (i, pre) in func.preconditions.iter().enumerate() {
            // Generate inputs that specifically violate this precondition
            let inputs = self.generate_violating(&pre.condition, &func.params);
            if let Some(inputs) = inputs {
                invalid.push((inputs, pre.message.clone().unwrap_or_else(||
                    format!("precondition {} violated", i + 1)
                )));
            }
        }

        invalid
    }

    fn random_inputs_for_params(&mut self, params: &[(String, Ty)]) -> Vec<Value> {
        params.iter().map(|(_, ty)| self.random_value(ty)).collect()
    }

    fn random_value(&mut self, ty: &Ty) -> Value {
        match ty {
            Ty::Int => Value::Int(self.rng.gen_range(-100..100)),
            Ty::Bool => Value::Bool(self.rng.gen()),
            Ty::Str => Value::Str(self.random_string()),
            Ty::List(inner) => {
                let len = self.rng.gen_range(0..5);
                Value::Array((0..len).map(|_| self.random_value(inner)).collect())
            }
            // ... other types
        }
    }
}
```

**2. Smart generation for common patterns:**

```rust
fn generate_violating(&mut self, condition: &Expr, params: &[(String, Ty)]) -> Option<Vec<Value>> {
    // Recognize common patterns and generate targeted violations
    match &condition.kind {
        // x > 0  →  generate x = 0 or x = -1
        ExprKind::Binary(
            box Expr { kind: ExprKind::Ident(var), .. },
            BinOp::Gt,
            box Expr { kind: ExprKind::Literal(Literal { kind: LiteralKind::Int(n), .. }), .. }
        ) => {
            let mut inputs = self.random_inputs_for_params(params);
            if let Some(idx) = params.iter().position(|(name, _)| name == &var.name) {
                inputs[idx] = Value::Int(*n);  // Set to boundary value
            }
            Some(inputs)
        }

        // x != 0  →  generate x = 0
        ExprKind::Binary(
            box Expr { kind: ExprKind::Ident(var), .. },
            BinOp::Ne,
            box Expr { kind: ExprKind::Literal(Literal { kind: LiteralKind::Int(n), .. }), .. }
        ) => {
            let mut inputs = self.random_inputs_for_params(params);
            if let Some(idx) = params.iter().position(|(name, _)| name == &var.name) {
                inputs[idx] = Value::Int(*n);  // Set to forbidden value
            }
            Some(inputs)
        }

        // x.len() > 0  →  generate empty array
        ExprKind::Binary(
            box Expr { kind: ExprKind::MethodCall(recv, method, _), .. },
            BinOp::Gt,
            box Expr { kind: ExprKind::Literal(Literal { kind: LiteralKind::Int(0), .. }), .. }
        ) if method.name == "len" => {
            if let ExprKind::Ident(var) = &recv.kind {
                let mut inputs = self.random_inputs_for_params(params);
                if let Some(idx) = params.iter().position(|(name, _)| name == &var.name) {
                    inputs[idx] = Value::Array(vec![]);  // Empty array
                }
                return Some(inputs);
            }
            None
        }

        _ => None,  // Can't generate targeted violation
    }
}
```

**3. Running examples and capturing output:**

```rust
pub fn run_example(
    interp: &mut Interpreter,
    func_name: &str,
    inputs: &[Value],
    config: &SafetyConfig,
) -> ExampleResult {
    // SAFETY: Verification runs MUST be sandboxed
    // - No capabilities by default (unless explicitly overridden)
    // - Step limit enforced
    // - Timeout enforced
    if !config.allow_side_effects {
        interp.revoke_all_capabilities();
    }
    interp.set_max_steps(config.max_steps);

    let old_check = interp.check_contracts;
    interp.set_check_contracts(false);

    let result = with_timeout(config.timeout_ms, || {
        interp.call(func_name, inputs.to_vec())
    });

    interp.set_check_contracts(old_check);

    match result {
        Ok(Ok(value)) => ExampleResult::Success(value),
        Ok(Err(e)) => ExampleResult::Error(e.message),
        Err(_timeout) => ExampleResult::Timeout,
    }
}

/// Safety configuration for verification runs
pub struct SafetyConfig {
    /// Max interpreter steps per example (default: 10_000)
    pub max_steps: usize,
    /// Timeout per example in milliseconds (default: 1000)
    pub timeout_ms: u64,
    /// Max examples to generate per function (default: 20)
    pub max_examples: usize,
    /// Allow side effects (DANGEROUS - off by default)
    pub allow_side_effects: bool,
}

impl Default for SafetyConfig {
    fn default() -> Self {
        Self {
            max_steps: 10_000,
            timeout_ms: 1000,
            max_examples: 20,
            allow_side_effects: false,
        }
    }
}
```

### Safety Constraints for Verification

**CRITICAL:** Verification runs execute generated inputs against user code. Without safeguards, this could trigger arbitrary side effects (file writes, network calls, process spawns).

**Mandatory safety measures:**

| Constraint | Default | Flag to Override |
|------------|---------|------------------|
| Capabilities | None (revoked) | `--allow-side-effects` |
| Step limit | 10,000 steps | `--max-steps N` |
| Timeout | 1 second per example | `--timeout MS` |
| Max examples | 20 per function | `--examples <N>` |

**Implementation requirements:**

1. `verify --report` MUST call `interp.revoke_all_capabilities()` unless `--allow-side-effects` is explicitly set
2. `verify --report` MUST enforce step limits via `interp.set_max_steps()`
3. `verify --report` MUST wrap execution in a timeout
4. If `--allow-side-effects` is passed, emit a warning once: "WARNING: Running verification with side effects enabled"
5. Functions requiring capabilities (file/network) show as SKIP in report, not FAIL

**Example output with capability-requiring function:**
```
src/io_utils.forma
├─ read_config ................................ ⊘ SKIP (requires file capability)
├─ fetch_url .................................. ⊘ SKIP (requires network capability)
└─ pure_transform ............................. ✓ PASS (3 contracts)
```

### Acceptance Criteria (46.3)

1. `forma explain file.forma --examples` shows concrete examples
2. Valid examples satisfy all preconditions and show actual output
3. Invalid examples show what inputs would violate preconditions
4. Smart generation targets boundary cases (0, empty, etc.)
5. Deterministic output with `--seed` flag for reproducibility
6. Configurable example count with `--examples=N` (or `--max-examples N`)

---

## Task 46.4: Verification Report

**Priority:** P1

**Files:**
- `src/main.rs` — extend `verify` command
- `src/main.rs` — report generation + JSON/human formatting

### Purpose

Generate a trust summary for a project:

```bash
$ forma verify --report src/

═══════════════════════════════════════════════════════════════════
                    FORMA Verification Report
═══════════════════════════════════════════════════════════════════

src/math.forma
├─ abs ......................................... ✓ PASS (3 contracts)
├─ divide ...................................... ✓ PASS (2 contracts)
└─ sqrt ........................................ ✓ PASS (1 contract)

src/sort.forma
├─ bubble_sort ................................. ✓ PASS (4 contracts)
├─ quick_sort .................................. ✓ PASS (4 contracts)
└─ merge ....................................... ⚠ WARN (no contracts)

src/search.forma
├─ binary_search ............................... ✓ PASS (3 contracts)
└─ linear_search ............................... ✓ PASS (2 contracts)

═══════════════════════════════════════════════════════════════════
Summary
═══════════════════════════════════════════════════════════════════

  Functions:     8 total
  With contracts: 7 (87.5%)
  Verified:      7 ✓
  Skipped:       0 ⊘
  Warnings:      1 ⚠ (no contracts)
  Failures:      0 ✗

  Test coverage: 156 examples generated, all passed

═══════════════════════════════════════════════════════════════════
```

### Implementation

**1. Report data structures:**

```rust
#[derive(Serialize)]
pub struct VerificationReport {
    pub files: Vec<FileReport>,
    pub summary: ReportSummary,
}

#[derive(Serialize)]
pub struct FileReport {
    pub path: PathBuf,
    pub file_error: Option<String>,
    pub functions: Vec<FunctionReport>,
}

#[derive(Serialize)]
pub struct FunctionReport {
    pub name: String,
    pub status: VerificationStatus,
    pub contract_count: usize,
    pub examples_run: usize,
    pub examples_passed: usize,
    pub skip_reason: Option<String>,
    pub issues: Vec<String>,
}

#[derive(Serialize)]
pub enum VerificationStatus {
    Pass,
    Skip,  // Not executable under current capability sandbox
    Warn,  // No contracts, or untested paths
    Fail,  // Contract violation found
}

#[derive(Serialize)]
pub struct ReportSummary {
    pub total_functions: usize,
    pub functions_with_contracts: usize,
    pub verified: usize,
    pub skipped: usize,
    pub warnings: usize,
    pub failures: usize,
    pub total_examples: usize,
    pub examples_passed: usize,
}
```

**2. Report generation:**

```rust
pub struct ReportConfig {
    pub seed: u64,
    pub max_examples: usize,
    pub safety: SafetyConfig,
    pub format: ReportFormat,
}

pub fn generate_report(paths: &[PathBuf], config: &ReportConfig) -> VerificationReport {
    let mut files = Vec::new();

    for path in paths {
        let source = fs::read_to_string(path)?;
        let mut loader = ModuleLoader::from_source_file(path)?;
        let loaded = match loader.load_imports(&source) {
            Ok(loaded) => loaded,
            Err(e) => {
                files.push(FileReport {
                    path: path.clone(),
                    file_error: Some(format_module_error(path, &e, config.format)),
                    functions: vec![],
                });
                continue;
            }
        };

        let ast = parse(&loaded.merged_source)?;
        let mir = lower(&ast)?;
        let mut interp = Interpreter::new(mir)?;

        let mut functions = Vec::new();
        for (name, func) in &interp.program.functions {
            if name == "main" { continue; }  // Skip main

            let report = verify_function(&mut interp, name, func, config)?;
            functions.push(report);
        }

        files.push(FileReport {
            path: path.clone(),
            file_error: None,
            functions,
        });
    }

    let summary = compute_summary(&files);
    VerificationReport { files, summary }
}

fn verify_function(
    interp: &mut Interpreter,
    name: &str,
    func: &Function,
    config: &ReportConfig,
) -> FunctionReport {
    let contract_count = func.preconditions.len() + func.postconditions.len();

    if contract_count == 0 {
        return FunctionReport {
            name: name.to_string(),
            status: VerificationStatus::Warn,
            contract_count: 0,
            examples_run: 0,
            examples_passed: 0,
            skip_reason: None,
            issues: vec!["no contracts defined".to_string()],
        };
    }

    // Generate and run examples
    let mut gen = ExampleGenerator::new(config.seed);
    let inputs = gen.generate_valid_inputs(func, config.max_examples);

    let mut passed = 0;
    let mut issues = Vec::new();
    let mut skip_reason = None;

    for input in &inputs {
        match run_with_contracts(interp, name, input, &config.safety) {
            Ok(_) => passed += 1,
            Err(e) if is_capability_denied(&e) => {
                skip_reason = Some(e);
                break;
            }
            Err(e) => issues.push(e),
        }
    }

    FunctionReport {
        name: name.to_string(),
        status: if skip_reason.is_some() {
            VerificationStatus::Skip
        } else if issues.is_empty() {
            VerificationStatus::Pass
        } else {
            VerificationStatus::Fail
        },
        contract_count,
        examples_run: inputs.len(),
        examples_passed: passed,
        skip_reason,
        issues,
    }
}
```

**3. Output formats:**

```rust
impl VerificationReport {
    pub fn to_human(&self) -> String {
        // Pretty-printed tree with colors/unicode
    }

    pub fn to_json(&self) -> String {
        serde_json::to_string_pretty(self).unwrap()
    }

    pub fn to_markdown(&self) -> String {
        // GitHub-friendly markdown table
    }
}
```

### Acceptance Criteria (46.4)

1. `forma verify --report dir/` produces summary for all files
2. Functions categorized as PASS/WARN/FAIL
3. WARN for functions without contracts
4. FAIL if any generated example violates contracts
5. Summary statistics at end
6. `--format json` for CI integration
7. Non-zero exit code if any failures

---

## Task 46.5: Named Contract Patterns

**Priority:** P2

**Files:**
- `src/parser/parser.rs` — pattern recognition
- `src/contracts/patterns.rs` — new module for pattern definitions

### Purpose

Provide readable shorthand for common contract patterns:

```forma
# Instead of:
@pre(items.len() > 0)
@post(result >= 0)
@post(forall i in 0..result.len()-1: result[i] <= result[i+1])

# Write:
@nonempty(items)
@nonnegative(result)
@sorted(result)
```

### Built-in Patterns

| Pattern | Expands To |
|---------|------------|
| `@nonempty(x)` | `@pre(x.len() > 0)` |
| `@nonnegative(x)` | `x >= 0` (in @pre or @post context) |
| `@positive(x)` | `x > 0` |
| `@nonzero(x)` | `x != 0` |
| `@bounded(x, lo, hi)` | `x >= lo && x <= hi` |
| `@sorted(x)` | `forall i in 0..x.len()-1: x[i] <= x[i+1]` |
| `@sorted_desc(x)` | `forall i in 0..x.len()-1: x[i] >= x[i+1]` |
| `@unique(x)` | `forall i in 0..x.len(): forall j in 0..x.len(): i != j => x[i] != x[j]` |
| `@same_length(a, b)` | `a.len() == b.len()` |
| `@permutation(a, b)` | Same elements, possibly reordered |
| `@unchanged(x)` | `x == old(x)` (postcondition only) |
| `@pure` | No side effects (marker, not checked) |

### Implementation

**1. Pattern registry:**

```rust
pub struct PatternRegistry {
    patterns: HashMap<String, ContractPattern>,
}

pub struct ContractPattern {
    pub name: String,
    pub params: Vec<String>,  // Parameter names
    pub expansion: Box<dyn Fn(&[Expr]) -> Expr>,  // How to expand
    pub english: Box<dyn Fn(&[Expr]) -> String>,  // English translation
    pub context: PatternContext,  // Pre, Post, or Both
}

pub enum PatternContext {
    Pre,
    Post,
    Both,
}

impl PatternRegistry {
    pub fn builtin() -> Self {
        let mut reg = Self::new();

        reg.register("nonempty", &["x"], PatternContext::Pre,
            |args| /* x.len() > 0 */,
            |args| format!("{} must not be empty", args[0])
        );

        reg.register("nonnegative", &["x"], PatternContext::Both,
            |args| /* x >= 0 */,
            |args| format!("{} must be non-negative", args[0])
        );

        reg.register("sorted", &["x"], PatternContext::Both,
            |args| /* forall i in 0..x.len()-1: x[i] <= x[i+1] */,
            |args| format!("{} must be sorted in ascending order", args[0])
        );

        // ... more patterns

        reg
    }
}
```

**2. Parser integration:**

```rust
fn parse_contract_attr(&mut self) -> Result<Contract> {
    let name = self.parse_ident()?;

    // Check if this is a named pattern
    if let Some(pattern) = self.pattern_registry.get(&name.name) {
        let args = self.parse_pattern_args()?;
        let expanded = pattern.expand(&args);
        return Ok(Contract {
            condition: expanded,
            message: None,  // Pattern provides default message
            pattern_name: Some(name.name.clone()),
            span: self.current_span(),
        });
    }

    // Otherwise parse as regular @pre/@post
    // ...
}
```

**3. Explain integration:**

Named patterns get better English output:
```rust
fn explain_contract(contract: &Contract) -> String {
    if let Some(pattern_name) = &contract.pattern_name {
        // Use pattern's english function
        return pattern_registry.english(pattern_name, &contract.args);
    }
    // Fall back to expression-based explanation
    explain_expr(&contract.condition)
}
```

### Acceptance Criteria (46.5)

1. `@nonempty(x)` expands to `@pre(x.len() > 0)`
2. `@sorted(x)` expands to appropriate forall expression
3. Patterns work in both @pre and @post contexts (where appropriate)
4. `forma explain` shows pattern name alongside expansion
5. Error messages reference pattern name when violation occurs
6. At least 10 built-in patterns covering common cases
7. Clear error if pattern used in wrong context

---

## Verification Checklist

Run and include in PR summary:

```bash
# =============================================================================
# Standard Checks
# =============================================================================
cargo build --release
cargo test --all
cargo clippy --all-targets -- -D warnings
cargo clippy --all-features --all-targets -- -D warnings
cargo fmt --all -- --check

# =============================================================================
# Unit Tests (Contract-Specific)
# =============================================================================
cargo test contract_expression -- --nocapture
cargo test explain -- --nocapture
cargo test example_gen -- --nocapture
cargo test report -- --nocapture
cargo test pattern -- --nocapture

# =============================================================================
# Integration Tests (New FORMA Files)
# =============================================================================
# Use built binary, not system PATH
FORMA=./target/release/forma

$FORMA run tests/forma/test_contract_expressions.forma
$FORMA run tests/forma/test_patterns.forma
$FORMA run tests/forma/test_old_expr.forma
$FORMA run tests/forma/test_quantifiers.forma
$FORMA run tests/forma/test_contracts_optional.forma

# Verify contracts can be disabled
$FORMA run --no-check-contracts tests/fixtures/contract_fail.forma
echo "Exit code: $?"  # Should be 0

# =============================================================================
# CLI Command Tests
# =============================================================================
# Explain command
$FORMA explain tests/forma/test_contracts.forma
$FORMA explain --format json tests/forma/test_contracts.forma
$FORMA explain --format markdown tests/forma/test_contracts.forma
$FORMA explain tests/forma/test_contracts.forma --examples
$FORMA explain --function divide tests/forma/test_contracts.forma

# Verify command
$FORMA verify --report tests/forma/
$FORMA verify --report tests/forma/ --format json

# =============================================================================
# Showcase Examples
# =============================================================================
$FORMA run examples/showcase/17_verified_sort.forma
$FORMA run examples/showcase/18_verified_stack.forma
$FORMA run examples/showcase/19_verified_math.forma
$FORMA run examples/showcase/20_verification_demo.forma

$FORMA explain examples/showcase/17_verified_sort.forma --examples
$FORMA explain examples/showcase/19_verified_math.forma --examples

$FORMA verify --report examples/showcase/

# =============================================================================
# Full Test Script
# =============================================================================
bash scripts/test_contracts.sh

# =============================================================================
# Error Path Tests
# =============================================================================
# These should fail with specific error messages
# Pattern: run command, capture output, assert non-zero exit AND expected message

# Test 1: contract_fail.forma should fail with "Contract violation"
output=$(./target/release/forma run tests/fixtures/contract_fail.forma 2>&1) && \
    { echo "FAIL: Expected contract_fail.forma to fail but it succeeded"; exit 1; }
echo "$output" | grep -q "Contract violation" || \
    { echo "FAIL: Expected 'Contract violation' in output"; echo "$output"; exit 1; }

# Test 2: test_contract_errors.forma should fail with "Contract violation"
output=$(./target/release/forma run tests/forma/test_contract_errors.forma 2>&1) && \
    { echo "FAIL: Expected test_contract_errors.forma to fail but it succeeded"; exit 1; }
echo "$output" | grep -q "Contract violation" || \
    { echo "FAIL: Expected 'Contract violation' in output"; echo "$output"; exit 1; }

# =============================================================================
# CI Simulation
# =============================================================================
cargo test --test cli_tests
```

---

## Test Files to Create

### `tests/forma/test_patterns.forma`
```forma
# Test named contract patterns

@nonempty(items)
@sorted(result)
@same_length(items, result)
f sort(items: [Int]) -> [Int]
    # ... implementation

@positive(n)
@nonnegative(result)
f factorial(n: Int) -> Int
    if n <= 1 then 1
    else n * factorial(n - 1)

@nonempty(items)
@post(result in items)
@post(forall x in items: result <= x)
f find_min(items: [Int]) -> Int
    m min := items[0]
    for x in items
        if x < min then min = x
    min

f main() -> Int
    # Test that patterns work
    _ := sort([3, 1, 4, 1, 5])
    _ := factorial(5)
    _ := find_min([3, 1, 4])
    0
```

### `tests/forma/test_old_expr.forma`
```forma
# Test old() in postconditions

@post(result.len() == old(items).len())
f double_all(items: [Int]) -> [Int]
    m result := []
    for x in items
        result = result + [x * 2]
    result

@post(result == old(a) + old(b))
f add(a: Int, b: Int) -> Int
    a + b

f main() -> Int
    _ := double_all([1, 2, 3])
    _ := add(2, 3)
    0
```

### `tests/forma/test_quantifiers.forma`
```forma
# Test forall/exists in contracts

@post(forall x in result: x >= 0)
f abs_all(items: [Int]) -> [Int]
    m result := []
    for x in items
        result = result + [if x < 0 then -x else x]
    result

@pre(forall i in 0..items.len(): items[i] >= 0)
@post(result >= 0)
f sum_positive(items: [Int]) -> Int
    m total := 0
    for x in items
        total = total + x
    total

@post(exists x in result: x == target)
f with_element(items: [Int], target: Int) -> [Int]
    items + [target]

f main() -> Int
    _ := abs_all([-1, 2, -3])
    _ := sum_positive([1, 2, 3])
    _ := with_element([1, 2], 3)
    0
```

---

## Testing Regimen

This sprint requires comprehensive testing at multiple levels to ensure correctness.

### Unit Tests (Rust)

**Location:** `src/mir/interp.rs`, `src/main.rs`, `src/module/loader.rs`

| Test Category | What to Test | Min Coverage |
|---------------|--------------|--------------|
| Contract expression eval | Each new expression type (index, tuple field, old, forall, exists, contains) | 100% of new code paths |
| Edge cases | Empty arrays, out-of-bounds, type mismatches | All error branches |
| `old()` semantics | Value capture timing, nested old(), mutation handling | 10+ scenarios |
| Quantifier evaluation | Empty iteration, single element, all true, one false, exists early exit | 8+ scenarios per quantifier |
| Explain translation | Each pattern type, fallback behavior, special idioms | 15+ patterns |
| Example generation | Valid inputs, boundary violations, type coverage | 10+ generation scenarios |
| Report aggregation | Pass/Skip/Warn/Fail counting, summary stats | 5+ report scenarios |
| Import resolution regression | Missing import in `verify --report` and `explain --examples` emits proper error | 4+ scenarios |

**Test structure in `interp.rs`:**
```rust
#[cfg(test)]
mod contract_expression_tests {
    // 46.1.1 Array indexing
    #[test] fn test_contract_array_index_valid() { ... }
    #[test] fn test_contract_array_index_out_of_bounds() { ... }
    #[test] fn test_contract_array_index_negative() { ... }
    #[test] fn test_contract_string_index() { ... }

    // 46.1.2 Tuple field access
    #[test] fn test_contract_tuple_field_valid() { ... }
    #[test] fn test_contract_tuple_field_out_of_bounds() { ... }

    // 46.1.3 old() expressions
    #[test] fn test_old_simple_value() { ... }
    #[test] fn test_old_array_mutation() { ... }
    #[test] fn test_old_nested_field() { ... }
    #[test] fn test_old_not_available_in_precondition() { ... }

    // 46.1.4 Quantifiers
    #[test] fn test_forall_empty_collection() { ... }
    #[test] fn test_forall_all_true() { ... }
    #[test] fn test_forall_one_false() { ... }
    #[test] fn test_forall_range_iteration() { ... }
    #[test] fn test_exists_found_early() { ... }
    #[test] fn test_exists_not_found() { ... }
    #[test] fn test_exists_empty_collection() { ... }

    // 46.1.5 Contains
    #[test] fn test_contains_array_found() { ... }
    #[test] fn test_contains_array_not_found() { ... }
    #[test] fn test_contains_string_char() { ... }
    #[test] fn test_contains_string_substring() { ... }
}
```

### Integration Tests (FORMA files)

**Location:** `tests/forma/`

| Test File | Purpose | Key Scenarios |
|-----------|---------|---------------|
| `test_contract_expressions.forma` | All new expression types | Index, tuple, old, forall, exists |
| `test_patterns.forma` | Named patterns | All 12 built-in patterns |
| `test_old_expr.forma` | `old()` semantics | Value capture, mutation, nested |
| `test_quantifiers.forma` | Quantifier behavior | forall/exists with ranges and collections |
| `test_contracts_optional.forma` | Optional contract behavior | Mixed contracted/uncontracted functions |
| `test_contract_errors.forma` | Error paths | Violations produce correct messages |

**Integration test runner script (`scripts/test_contracts.sh`):**
```bash
#!/bin/bash
set -e

# Use built binary, not system PATH
FORMA="${FORMA:-./target/release/forma}"

# Verify binary exists
if [[ ! -x "$FORMA" ]]; then
    echo "ERROR: forma binary not found at $FORMA"
    echo "Run 'cargo build --release' first"
    exit 1
fi

echo "Using forma binary: $FORMA"
echo ""

echo "=== Contract Expression Tests ==="
$FORMA run tests/forma/test_contract_expressions.forma
$FORMA run --no-check-contracts tests/forma/test_contract_expressions.forma

echo "=== Pattern Tests ==="
$FORMA run tests/forma/test_patterns.forma

echo "=== Old Expression Tests ==="
$FORMA run tests/forma/test_old_expr.forma

echo "=== Quantifier Tests ==="
$FORMA run tests/forma/test_quantifiers.forma

echo "=== Optional Contracts Tests ==="
$FORMA run tests/forma/test_contracts_optional.forma

echo "=== Explain Command Tests ==="
$FORMA explain tests/forma/test_patterns.forma
$FORMA explain --format json tests/forma/test_patterns.forma > /dev/null
$FORMA explain tests/forma/test_patterns.forma --examples

echo "=== Verify Report Tests ==="
$FORMA verify --report tests/forma/ --format json > /dev/null
$FORMA verify --report tests/forma/

echo "=== Contract Error Tests ==="
# These should fail with specific error messages
# Pattern: capture output, check for non-zero exit AND expected message

output=$($FORMA run tests/forma/test_contract_errors.forma 2>&1) && \
    { echo "FAIL: Expected failure but got success"; exit 1; }
echo "$output" | grep -q "Contract violation" || \
    { echo "FAIL: Expected 'Contract violation' in output"; exit 1; }
echo "Contract error test passed"

echo ""
echo "All contract tests passed!"
```

### CLI Tests

**Location:** `tests/cli_tests.rs`

```rust
#[test]
fn test_explain_command_human_format() {
    let output = Command::new("forma")
        .args(["explain", "tests/fixtures/with_contracts.forma"])
        .output()
        .expect("failed to execute");
    assert!(output.status.success());
    assert!(String::from_utf8_lossy(&output.stdout).contains("Requires:"));
    assert!(String::from_utf8_lossy(&output.stdout).contains("Guarantees:"));
}

#[test]
fn test_explain_command_json_format() {
    let output = Command::new("forma")
        .args(["explain", "--format", "json", "tests/fixtures/with_contracts.forma"])
        .output()
        .expect("failed to execute");
    assert!(output.status.success());
    let json: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert!(json["functions"].is_array());
}

#[test]
fn test_explain_examples_flag() {
    let output = Command::new("forma")
        .args(["explain", "tests/fixtures/with_contracts.forma", "--examples"])
        .output()
        .expect("failed to execute");
    assert!(output.status.success());
    assert!(String::from_utf8_lossy(&output.stdout).contains("Examples:"));
}

#[test]
fn test_verify_report_pass() {
    let output = Command::new("forma")
        .args(["verify", "--report", "tests/fixtures/all_pass/"])
        .output()
        .expect("failed to execute");
    assert!(output.status.success());
    assert!(String::from_utf8_lossy(&output.stdout).contains("✓ PASS"));
}

#[test]
fn test_verify_report_warn_no_contracts() {
    let output = Command::new("forma")
        .args(["verify", "--report", "tests/fixtures/no_contracts/"])
        .output()
        .expect("failed to execute");
    assert!(output.status.success());  // Warnings don't fail
    assert!(String::from_utf8_lossy(&output.stdout).contains("⚠ WARN"));
}

#[test]
fn test_verify_report_fail() {
    let output = Command::new("forma")
        .args(["verify", "--report", "tests/fixtures/contract_bug/"])
        .output()
        .expect("failed to execute");
    assert!(!output.status.success());  // Failures exit non-zero
    assert!(String::from_utf8_lossy(&output.stdout).contains("✗ FAIL"));
}

#[test]
fn test_verify_report_missing_import_json_error() {
    let output = Command::new("forma")
        .args(["verify", "--report", "--format", "json", "tests/fixtures/missing_import.forma"])
        .output()
        .expect("failed to execute");
    assert!(!output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stdout.contains("\"success\":false")
            || stdout.contains("\"errors\"")
            || stderr.contains("\"success\":false")
            || stderr.contains("\"errors\"")
    );
}

#[test]
fn test_explain_examples_missing_import_json_error() {
    let output = Command::new("forma")
        .args(["explain", "tests/fixtures/missing_import.forma", "--examples", "--format", "json"])
        .output()
        .expect("failed to execute");
    assert!(!output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stdout.contains("\"success\":false")
            || stdout.contains("\"errors\"")
            || stderr.contains("\"success\":false")
            || stderr.contains("\"errors\"")
    );
}

#[test]
fn test_no_check_contracts_flag() {
    // This file has a precondition violation
    let output = Command::new("forma")
        .args(["run", "--no-check-contracts", "tests/fixtures/contract_fail.forma"])
        .output()
        .expect("failed to execute");
    assert!(output.status.success());  // Should succeed with contracts disabled
}
```

### Property-Based Tests (Optional but Recommended)

Using `proptest` or `quickcheck` for fuzz testing:

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn forall_true_on_empty_is_true(predicate in any::<bool>()) {
        // forall x in []: P(x) should always be true
        let result = eval_forall(&[], |_| predicate);
        prop_assert!(result);
    }

    #[test]
    fn exists_false_on_empty_is_false(predicate in any::<bool>()) {
        // exists x in []: P(x) should always be false
        let result = eval_exists(&[], |_| predicate);
        prop_assert!(!result);
    }

    #[test]
    fn old_captures_value_before_mutation(
        initial in -1000i64..1000,
        delta in -100i64..100
    ) {
        // old(x) should return initial, not initial + delta
        let captured = simulate_old_capture(initial);
        let mutated = initial + delta;
        prop_assert_eq!(captured, initial);
        prop_assert_ne!(captured, mutated);  // Unless delta == 0
    }
}
```

### CI Integration

Add to `.github/workflows/ci.yml`:

```yaml
  contract-tests:
    name: Contract & Verification UX Tests
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable

      - name: Build
        run: cargo build --release

      - name: Unit tests (contract-related)
        run: cargo test contract --all

      - name: Integration tests
        run: bash scripts/test_contracts.sh
        env:
          FORMA: ./target/release/forma

      - name: CLI tests
        run: cargo test --test cli_tests

      - name: Verify showcase examples
        run: |
          ./target/release/forma verify --report examples/showcase/
          for f in examples/showcase/*.forma; do
            ./target/release/forma explain "$f"
          done
```

---

## Showcase Examples

Create polished examples demonstrating the verification UX for the `examples/showcase/` directory.

### `examples/showcase/17_verified_sort.forma`

```forma
# Showcase: Verified Sorting Algorithm
# Demonstrates contracts that specify sorting behavior

# =============================================================================
# Insertion Sort with Full Specification
# =============================================================================

@nonempty(items)
@post(result.len() == items.len())
@post(forall i in 0..result.len()-1: result[i] <= result[i+1])
@post(forall x in items: x in result)
f insertion_sort(items: [Int]) -> [Int]
    m sorted := [items[0]]

    for i in 1..items.len()
        m val := items[i]
        m inserted := false
        m new_sorted := []

        for j in 0..sorted.len()
            if !inserted && val <= sorted[j]
                new_sorted = new_sorted + [val]
                inserted = true
            new_sorted = new_sorted + [sorted[j]]

        if !inserted
            new_sorted = new_sorted + [val]

        sorted = new_sorted

    sorted

# =============================================================================
# Binary Search (requires sorted input)
# =============================================================================

@nonempty(items)
@pre(forall i in 0..items.len()-1: items[i] <= items[i+1])
@post(result >= 0 => items[result] == target)
@post(result < 0 => forall x in items: x != target)
f binary_search(items: [Int], target: Int) -> Int
    m lo := 0
    m hi := items.len() - 1

    wh lo <= hi
        m mid := (lo + hi) / 2
        if items[mid] == target
            ret mid
        else if items[mid] < target
            lo = mid + 1
        else
            hi = mid - 1

    -1  # Not found

# =============================================================================
# Demo
# =============================================================================

f main() -> Int
    unsorted := [64, 34, 25, 12, 22, 11, 90]

    print("Original: ")
    print(unsorted)

    sorted := insertion_sort(unsorted)
    print("Sorted: ")
    print(sorted)

    idx := binary_search(sorted, 25)
    print("Index of 25: ")
    print(idx)

    idx2 := binary_search(sorted, 100)
    print("Index of 100 (not found): ")
    print(idx2)

    0
```

### `examples/showcase/18_verified_stack.forma`

```forma
# Showcase: Verified Stack Implementation
# Demonstrates contracts for data structure invariants

# =============================================================================
# Stack with Push/Pop Contracts
# =============================================================================

s Stack
    items: [Int]
    capacity: Int

@post(result.items.len() == 0)
@post(result.capacity == cap)
f stack_new(cap: Int) -> Stack
    Stack(items: [], capacity: cap)

@pre(s.items.len() < s.capacity)
@post(result.items.len() == old(s).items.len() + 1)
@post(result.items[result.items.len() - 1] == value)
f stack_push(s: Stack, value: Int) -> Stack
    Stack(items: s.items + [value], capacity: s.capacity)

@nonempty(s.items)
@post(result.0.items.len() == old(s).items.len() - 1)
@post(result.1 == old(s).items[old(s).items.len() - 1])
f stack_pop(s: Stack) -> (Stack, Int)
    last_idx := s.items.len() - 1
    value := s.items[last_idx]

    m new_items := []
    for i in 0..last_idx
        new_items = new_items + [s.items[i]]

    (Stack(items: new_items, capacity: s.capacity), value)

@post(result == s.items.len() == 0)
f stack_is_empty(s: Stack) -> Bool
    s.items.len() == 0

@post(result == s.items.len() == s.capacity)
f stack_is_full(s: Stack) -> Bool
    s.items.len() == s.capacity

# =============================================================================
# Demo
# =============================================================================

f main() -> Int
    m stack := stack_new(5)
    print("Created stack with capacity 5")

    stack = stack_push(stack, 10)
    stack = stack_push(stack, 20)
    stack = stack_push(stack, 30)
    print("Pushed 10, 20, 30")

    (stack, val) := stack_pop(stack)
    print("Popped: ")
    print(val)

    (stack, val2) := stack_pop(stack)
    print("Popped: ")
    print(val2)

    0
```

### `examples/showcase/19_verified_math.forma`

```forma
# Showcase: Verified Mathematical Functions
# Demonstrates contracts for numerical algorithms

# =============================================================================
# Factorial with Termination Guarantee
# =============================================================================

@pre(n >= 0)
@pre(n <= 20)  # Prevent overflow
@post(result >= 1)
@post(n == 0 => result == 1)
@post(n > 0 => result == n * factorial(n - 1))
f factorial(n: Int) -> Int
    if n <= 1 then 1
    else n * factorial(n - 1)

# =============================================================================
# Greatest Common Divisor (Euclidean Algorithm)
# =============================================================================

@positive(a)
@positive(b)
@post(result > 0)
@post(a % result == 0)
@post(b % result == 0)
f gcd(a: Int, b: Int) -> Int
    if b == 0 then a
    else gcd(b, a % b)

# =============================================================================
# Integer Square Root (Floor)
# =============================================================================

@nonnegative(n)
@post(result >= 0)
@post(result * result <= n)
@post((result + 1) * (result + 1) > n)
f isqrt(n: Int) -> Int
    if n == 0 then ret 0

    m x := n
    m y := (x + 1) / 2

    wh y < x
        x = y
        y = (x + n / x) / 2

    x

# =============================================================================
# Absolute Value
# =============================================================================

@post(result >= 0)
@post(result == x || result == -x)
f abs(x: Int) -> Int
    if x < 0 then -x else x

# =============================================================================
# Safe Division (returns Option)
# =============================================================================

@post(b == 0 => result == None)
@post(b != 0 => result == Some(a / b))
f safe_div(a: Int, b: Int) -> Int?
    if b == 0 then None
    else Some(a / b)

# =============================================================================
# Demo
# =============================================================================

f main() -> Int
    print("factorial(5) = ")
    print(factorial(5))

    print("gcd(48, 18) = ")
    print(gcd(48, 18))

    print("isqrt(50) = ")
    print(isqrt(50))

    print("abs(-42) = ")
    print(abs(-42))

    print("safe_div(10, 3) = ")
    m result := safe_div(10, 3)
    if is_some(result) then print(unwrap(result))

    print("safe_div(10, 0) = ")
    m result2 := safe_div(10, 0)
    if is_none(result2) then print("None")

    0
```

### `examples/showcase/20_verification_demo.forma`

```forma
# Showcase: Verification Tooling Demo
# Run with: forma explain examples/showcase/20_verification_demo.forma --examples
# Run with: forma verify --report examples/showcase/

# =============================================================================
# Functions with Various Contract Patterns
# =============================================================================

# Simple precondition
@pre(x > 0)
f require_positive(x: Int) -> Int
    x * 2

# Simple postcondition
@post(result >= 0)
f always_nonnegative(x: Int) -> Int
    x * x

# Both pre and post
@pre(n >= 0)
@post(result >= 1)
f power_of_two(n: Int) -> Int
    if n == 0 then 1
    else 2 * power_of_two(n - 1)

# Multiple preconditions
@pre(a >= 0)
@pre(b >= 0)
@pre(a + b > 0)
f safe_average(a: Int, b: Int) -> Int
    (a + b) / 2

# Collection contracts
@nonempty(items)
@post(forall x in result: x >= 0)
f abs_all(items: [Int]) -> [Int]
    m result := []
    for x in items
        result = result + [if x < 0 then -x else x]
    result

# Using old() for mutation tracking
@post(result.len() == old(items).len() + 1)
@post(result[result.len() - 1] == item)
f append(items: [Int], item: Int) -> [Int]
    items + [item]

# No contracts (will show WARN in verify report)
f unspecified_function(x: Int) -> Int
    x + 1

# =============================================================================
# Main
# =============================================================================

f main() -> Int
    print("=== Verification Demo ===")
    print("")
    print("Run 'forma explain <file> --examples' on this file to see:")
    print("  - Human-readable contract explanations")
    print("  - Auto-generated examples")
    print("")
    print("Run 'forma verify --report' to see:")
    print("  - Pass/Skip/Warn/Fail status for each function")
    print("  - Coverage of contracted vs uncontracted functions")
    print("")

    # Actually run the functions
    _ := require_positive(5)
    _ := always_nonnegative(-3)
    _ := power_of_two(4)
    _ := safe_average(10, 20)
    _ := abs_all([-1, 2, -3])
    _ := append([1, 2], 3)
    _ := unspecified_function(42)

    print("All functions executed successfully!")
    0
```

### Update `examples/showcase/README.md`

Add entries for new showcase files:

```markdown
## Verification UX Examples (Sprint 46)

| # | File | Demonstrates |
|---|------|--------------|
| 17 | `17_verified_sort.forma` | Sorting with full specification (pre/post, forall, permutation) |
| 18 | `18_verified_stack.forma` | Data structure invariants with old() |
| 19 | `19_verified_math.forma` | Mathematical functions with contracts |
| 20 | `20_verification_demo.forma` | Tooling demo - run with `forma explain` and `forma verify` |

### Running the Examples

```bash
# See human-readable contract explanations
forma explain examples/showcase/17_verified_sort.forma

# See auto-generated examples
forma explain examples/showcase/19_verified_math.forma --examples

# Get JSON output for tooling
forma explain --format json examples/showcase/17_verified_sort.forma

# Verify all showcase examples
forma verify --report examples/showcase/

# Run a specific example
forma run examples/showcase/20_verification_demo.forma
```
```

---

## Out of Scope

1. Static/SMT verification (future sprint)
2. User-defined contract patterns (future enhancement)
3. Contract inheritance for traits/interfaces
4. IDE/LSP integration (separate sprint, uses JSON output from this sprint)
5. Async-specific contracts (e.g., deadlock freedom)

---

## Definition of Done

### Core Functionality
1. All new contract expressions work in runtime checking
2. `forma explain` produces clear English summaries
3. `forma explain <file> --examples` shows concrete illustrations
4. `forma verify --report` produces trust summary
5. At least 10 named patterns implemented
6. Contracts remain optional — code without contracts compiles and runs

### Testing
7. Unit tests cover all new contract expression types (100% path coverage)
8. Integration tests pass for all new `.forma` test files
9. CLI tests verify all new command options
10. `scripts/test_contracts.sh` passes completely
11. CI workflow includes contract test job

### Showcase
12. 4 new showcase examples created (17-20)
13. All showcase examples pass `forma verify`
14. `examples/showcase/README.md` updated with new entries
15. `forma explain <file> --examples` works on all showcase files

### Documentation
16. CLI `--help` updated for new commands/flags
17. Error messages are clear and actionable

---

## Agent Instructions

1. Implement in task order (46.1 is foundation for others)
2. Write tests alongside implementation, not after
3. Prefer clear error messages over silent failures
4. Keep English translations natural, not robotic
5. JSON output must be stable for tooling consumption
6. Run full verification checklist before marking complete
