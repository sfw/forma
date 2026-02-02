# Sprint 12: Contextual Keywords Fix

**Priority:** Critical - Pillar 1 Alignment
**Date:** January 25, 2026

---

## Problem Statement

FORMA's short keywords (`m`, `s`, `f`, `e`, `t`, `i`) conflict with natural variable names that AI models commonly generate:

```forma
f map_get_or[K, V](m: {K: V}, key: K, default: V) -> V {
    m map_get(m, key) {  # ERROR: parser sees "match map_get(match, key)"
        Some(v) => v,
        None => default
    }
}
```

This violates **Pillar 1 (AI Code Generation First)** - AI-generated code should "just work" without learning FORMA-specific naming conventions.

### Common Conflicts

| Keyword | Meaning | Natural Variable Use |
|---------|---------|---------------------|
| `m` | match | `m` for map |
| `s` | struct | `s` for string |
| `e` | enum | `e` for error, element |
| `t` | trait | `t` for type, temp |
| `i` | impl | `i` for index, iterator |
| `f` | function | `f` for file, func |

---

## Solution: Context-Aware Keyword Resolution

**Principle:** Keywords are only keywords in positions where they make syntactic sense. Everywhere else, they're identifiers.

### Keyword Context Rules

| Keyword | Only a keyword when... |
|---------|----------------------|
| `f` | At start of item (top-level or in impl block) |
| `as f` | At start of item (async function) |
| `s` | At start of item |
| `e` | At start of item |
| `t` | At start of item |
| `i` | At start of item |
| `m` | At start of expression statement |
| `if` | At start of expression |
| `for` | At start of expression statement |
| `while` | At start of expression statement |
| `ret` | At start of statement |
| `sp` | At start of expression (spawn) |
| `as` | After `f` or in type cast position |

### Where Keywords Become Identifiers

1. **Function parameters:** `f foo(m: Map, s: Str, e: Error)`
2. **Variable bindings:** `m := map_new()`
3. **Struct fields:** `s Point { x: Int, y: Int }`
4. **Expression arguments:** `vec_push(items, e)`
5. **Pattern bindings:** `Some(m) => ...`
6. **Type parameters:** `f foo[T, E](...)` (E is identifier, not enum)
7. **Field access:** `point.x` (x is always identifier)
8. **Method calls:** `v.m()` (m after dot is always identifier)

---

## Implementation

### Task 12.1: Add Lexer Mode/Context Tracking

**File:** `src/lexer/scanner.rs`

The lexer should emit contextual tokens that can be either keyword or identifier. Add a new token kind:

```rust
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // ... existing tokens ...

    // Contextual tokens - could be keyword OR identifier
    // Parser decides based on context
    ContextualF,      // f - function or identifier
    ContextualS,      // s - struct or identifier
    ContextualE,      // e - enum or identifier
    ContextualT,      // t - trait or identifier
    ContextualI,      // i - impl or identifier
    ContextualM,      // m - match or identifier

    // Or simpler: just emit as Ident and let parser check
}
```

**Alternative (simpler):** Emit single-letter keywords as `Ident` tokens, let parser decide:

```rust
fn scan_identifier(&mut self) -> Token {
    let text = self.consume_identifier();

    // Single-letter "keywords" are emitted as identifiers
    // Parser handles context-based interpretation
    let kind = match text.as_str() {
        // These are ALWAYS keywords (not ambiguous)
        "if" => TokenKind::If,
        "for" => TokenKind::For,
        "while" => TokenKind::While,
        "ret" => TokenKind::Ret,
        "true" => TokenKind::True,
        "false" => TokenKind::False,
        "as" => TokenKind::As,
        "sp" => TokenKind::Sp,
        "in" => TokenKind::In,

        // These are CONTEXTUAL - emit as Ident, parser decides
        "f" | "s" | "e" | "t" | "i" | "m" => TokenKind::Ident,

        // Regular identifiers
        _ => TokenKind::Ident,
    };

    Token::new(kind, text, self.current_span())
}
```

---

### Task 12.2: Update Parser for Context-Aware Resolution

**File:** `src/parser/parser.rs`

Add helper methods to check if an identifier should be treated as a keyword:

```rust
impl Parser {
    /// Check if current token is a contextual keyword in item position
    fn is_item_keyword(&self) -> bool {
        if let Some(token) = self.current() {
            if token.kind == TokenKind::Ident {
                match token.text.as_str() {
                    "f" => self.lookahead_is_function(),
                    "s" => self.lookahead_is_struct(),
                    "e" => self.lookahead_is_enum(),
                    "t" => self.lookahead_is_trait(),
                    "i" => self.lookahead_is_impl(),
                    _ => false,
                }
            } else {
                false
            }
        } else {
            false
        }
    }

    /// Check if 'f' starts a function definition
    /// f name(...) or f name[T](...)
    fn lookahead_is_function(&self) -> bool {
        // f must be followed by identifier (function name)
        if let Some(next) = self.peek(1) {
            next.kind == TokenKind::Ident
        } else {
            false
        }
    }

    /// Check if 's' starts a struct definition
    /// s Name { ... }
    fn lookahead_is_struct(&self) -> bool {
        if let Some(next) = self.peek(1) {
            // Struct name should be capitalized by convention
            // But we'll accept any identifier followed by { or [
            if next.kind == TokenKind::Ident {
                if let Some(after) = self.peek(2) {
                    matches!(after.kind, TokenKind::LBrace | TokenKind::LBracket)
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        }
    }

    /// Check if 'e' starts an enum definition
    fn lookahead_is_enum(&self) -> bool {
        if let Some(next) = self.peek(1) {
            if next.kind == TokenKind::Ident {
                if let Some(after) = self.peek(2) {
                    matches!(after.kind, TokenKind::LBrace | TokenKind::LBracket)
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        }
    }

    /// Check if 't' starts a trait definition
    fn lookahead_is_trait(&self) -> bool {
        if let Some(next) = self.peek(1) {
            if next.kind == TokenKind::Ident {
                if let Some(after) = self.peek(2) {
                    matches!(after.kind, TokenKind::LBrace | TokenKind::LBracket | TokenKind::Colon)
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        }
    }

    /// Check if 'i' starts an impl block
    fn lookahead_is_impl(&self) -> bool {
        if let Some(next) = self.peek(1) {
            // i TypeName { or i TypeName : Trait {
            next.kind == TokenKind::Ident
        } else {
            false
        }
    }

    /// Check if 'm' is the match keyword (at expression statement start)
    fn is_match_keyword(&self) -> bool {
        if let Some(token) = self.current() {
            if token.kind == TokenKind::Ident && token.text == "m" {
                // m is match if followed by an expression (not :=, not :, not ,)
                if let Some(next) = self.peek(1) {
                    // If next is := or : or , or ), this is an identifier
                    !matches!(next.kind,
                        TokenKind::ColonEq |    // m := ...
                        TokenKind::Colon |      // m: Type
                        TokenKind::Comma |      // (m, n)
                        TokenKind::RParen |     // fn(m)
                        TokenKind::RBracket |   // [m]
                        TokenKind::RBrace |     // {m}
                        TokenKind::Eq |         // m = ... (reassignment)
                        TokenKind::Dot          // m.field
                    )
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        }
    }
}
```

---

### Task 12.3: Update Item Parsing

**File:** `src/parser/parser.rs`

Update `parse_item` to use contextual checks:

```rust
fn parse_item(&mut self) -> Result<Item, ParseError> {
    // Handle async functions: as f name(...)
    if self.check_ident("as") {
        if self.peek_ident(1, "f") {
            return self.parse_async_function();
        }
    }

    // Check for contextual keywords at item position
    if let Some(token) = self.current() {
        if token.kind == TokenKind::Ident {
            match token.text.as_str() {
                "f" if self.lookahead_is_function() => {
                    return self.parse_function();
                }
                "s" if self.lookahead_is_struct() => {
                    return self.parse_struct();
                }
                "e" if self.lookahead_is_enum() => {
                    return self.parse_enum();
                }
                "t" if self.lookahead_is_trait() => {
                    return self.parse_trait();
                }
                "i" if self.lookahead_is_impl() => {
                    return self.parse_impl();
                }
                _ => {}
            }
        }
    }

    // Not an item keyword - parse as expression statement
    self.parse_statement()
}

/// Helper to check if current token is specific identifier
fn check_ident(&self, name: &str) -> bool {
    self.current()
        .map(|t| t.kind == TokenKind::Ident && t.text == name)
        .unwrap_or(false)
}

/// Helper to check if peek(n) is specific identifier
fn peek_ident(&self, n: usize, name: &str) -> bool {
    self.peek(n)
        .map(|t| t.kind == TokenKind::Ident && t.text == name)
        .unwrap_or(false)
}
```

---

### Task 12.4: Update Expression Parsing for Match

**File:** `src/parser/parser.rs`

Update expression statement parsing to handle `m` contextually:

```rust
fn parse_expression_statement(&mut self) -> Result<Stmt, ParseError> {
    // Check for match expression: m expr { ... }
    if self.is_match_keyword() {
        return self.parse_match_expression();
    }

    // Otherwise parse as normal expression
    let expr = self.parse_expression()?;
    Ok(Stmt::Expr(expr))
}

fn parse_match_expression(&mut self) -> Result<Expr, ParseError> {
    // Consume 'm' (we already verified it's the match keyword)
    self.expect_ident("m")?;

    // Parse the scrutinee expression
    let scrutinee = self.parse_expression()?;

    // Parse match arms in braces
    self.expect(TokenKind::LBrace)?;
    let arms = self.parse_match_arms()?;
    self.expect(TokenKind::RBrace)?;

    Ok(Expr::Match(Box::new(scrutinee), arms))
}
```

---

### Task 12.5: Ensure Identifiers Work Everywhere

**File:** `src/parser/parser.rs`

Make sure these positions always treat single letters as identifiers:

```rust
fn parse_parameter(&mut self) -> Result<Param, ParseError> {
    // Parameter name - always an identifier, even if it's m, s, f, etc.
    let name = self.parse_ident()?;  // This should accept ANY identifier

    self.expect(TokenKind::Colon)?;
    let ty = self.parse_type()?;

    Ok(Param { name, ty })
}

fn parse_ident(&mut self) -> Result<Ident, ParseError> {
    let token = self.current()
        .ok_or_else(|| self.error("expected identifier"))?;

    // Accept Ident tokens (which now includes m, s, f, e, t, i)
    if token.kind == TokenKind::Ident {
        self.advance();
        Ok(Ident::new(token.text.clone(), token.span))
    } else {
        Err(self.error(&format!("expected identifier, found {:?}", token.kind)))
    }
}

fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
    // Pattern bindings are always identifiers
    if let Some(token) = self.current() {
        if token.kind == TokenKind::Ident {
            // Could be: variable binding, enum variant, struct name
            // Single letters are always variable bindings in patterns
            // (Enum variants are PascalCase like Some, None, Ok, Err)
            // ...
        }
    }
    // ...
}
```

---

## Test Cases

### Task 12.6: Add Parser Tests

**File:** `tests/parser/contextual_keywords.rs` (new file)

```rust
#[test]
fn test_m_as_parameter() {
    let code = r#"
        f get_value(m: Map[Str, Int], key: Str) -> Int? {
            map_get(m, key)
        }
    "#;
    assert!(parse(code).is_ok());
}

#[test]
fn test_m_as_variable_and_match() {
    let code = r#"
        f example() {
            m := map_new()
            m map_get(m, "key") {
                Some(v) => v,
                None => 0
            }
        }
    "#;
    assert!(parse(code).is_ok());
}

#[test]
fn test_s_as_parameter() {
    let code = r#"
        f process(s: Str) -> Int {
            str_len(s)
        }
    "#;
    assert!(parse(code).is_ok());
}

#[test]
fn test_e_as_parameter() {
    let code = r#"
        f handle_error(e: Error) -> Str {
            error_message(e)
        }
    "#;
    assert!(parse(code).is_ok());
}

#[test]
fn test_f_as_parameter() {
    let code = r#"
        f apply(f: (Int) -> Int, x: Int) -> Int {
            f(x)
        }
    "#;
    assert!(parse(code).is_ok());
}

#[test]
fn test_i_as_loop_variable() {
    let code = r#"
        f sum_range(n: Int) -> Int {
            total := 0
            for i in range(n) {
                total = total + i
            }
            total
        }
    "#;
    assert!(parse(code).is_ok());
}

#[test]
fn test_t_as_type_parameter() {
    let code = r#"
        f identity[T](t: T) -> T {
            t
        }
    "#;
    assert!(parse(code).is_ok());
}

#[test]
fn test_all_keywords_as_struct_fields() {
    let code = r#"
        s Example {
            f: Int,
            s: Str,
            e: Bool,
            t: Float,
            i: Int,
            m: Map[Str, Int]
        }
    "#;
    assert!(parse(code).is_ok());
}

#[test]
fn test_keywords_in_tuple_destructure() {
    let code = r#"
        f example() {
            (m, s, e) := (1, "hello", true)
            m + str_len(s)
        }
    "#;
    assert!(parse(code).is_ok());
}

#[test]
fn test_match_still_works() {
    let code = r#"
        f example(x: Int) -> Str {
            m x {
                0 => "zero",
                1 => "one",
                _ => "many"
            }
        }
    "#;
    assert!(parse(code).is_ok());
}
```

### Task 12.7: Add Integration Tests

**File:** `tests/forma/test_contextual_keywords.forma` (new file)

```forma
# Test that single-letter keywords work as identifiers

f test_m_as_map_param() {
    m := {"a": 1, "b": 2}
    result := get_from_map(m, "a")
    assert_eq(result, Some(1))
}

f get_from_map(m: {Str: Int}, key: Str) -> Int? {
    map_get(m, key)
}

f test_m_as_var_then_match() {
    m := map_new()
    m = map_insert(m, "key", 42)

    # m as match keyword, m as variable
    result := m map_get(m, "key") {
        Some(v) => v,
        None => 0
    }
    assert_eq(result, 42)
}

f test_s_as_string_param() {
    s := "hello"
    assert_eq(get_length(s), 5)
}

f get_length(s: Str) -> Int {
    str_len(s)
}

f test_e_as_error() {
    e := "something went wrong"
    assert_eq(format_error(e), "Error: something went wrong")
}

f format_error(e: Str) -> Str {
    "Error: " + e
}

f test_f_as_function_param() {
    f := |x: Int| -> Int { x * 2 }
    assert_eq(apply_func(f, 21), 42)
}

f apply_func(f: (Int) -> Int, x: Int) -> Int {
    f(x)
}

f test_i_as_index() {
    items := [10, 20, 30]
    total := 0
    for i in range(3) {
        total = total + items[i]
    }
    assert_eq(total, 60)
}

f test_all_letters_in_struct() {
    s Data {
        f: Int,
        s: Str,
        e: Bool,
        t: Float,
        i: Int,
        m: {Str: Int}
    }

    d := Data {
        f: 1,
        s: "test",
        e: true,
        t: 3.14,
        i: 42,
        m: {"x": 100}
    }

    assert_eq(d.f, 1)
    assert_eq(d.s, "test")
    assert_eq(d.e, true)
    assert_eq(d.i, 42)
}

f test_tuple_destructure() {
    (m, s, e) := (1, "two", 3.0)
    assert_eq(m, 1)
    assert_eq(s, "two")
}

f test_match_pattern_binding() {
    opt := Some(42)
    m opt {
        Some(m) => assert_eq(m, 42),  # m as pattern binding
        None => assert_true(false)
    }
}
```

---

## Definition of Done

- [ ] Lexer emits single-letter keywords as `Ident` tokens
- [ ] Parser uses lookahead to determine keyword vs identifier
- [ ] All existing tests still pass
- [ ] New contextual keyword tests pass
- [ ] `m` works as parameter name AND as match keyword
- [ ] `s`, `e`, `t`, `i`, `f` work as variable/parameter names
- [ ] Struct fields can use any single letter
- [ ] Pattern bindings can use any single letter
- [ ] Error messages remain helpful

---

## Notes for Implementation

1. **Start with lexer changes** - Make it emit `Ident` for single-letter keywords
2. **Update parser helpers** - Add the lookahead functions
3. **Update item parsing** - Use contextual checks for `f`, `s`, `e`, `t`, `i`
4. **Update expression parsing** - Use contextual check for `m` (match)
5. **Run existing tests** - They should still pass
6. **Add new tests** - Verify contextual behavior works

The key insight is that the *parser* has enough context to disambiguate. We don't need complex lexer modes. Just emit ambiguous tokens as `Ident` and let the parser decide based on position and lookahead.

---

*Pillar 1: AI Code Generation First - Natural variable names should "just work"*
