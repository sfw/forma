# Sprint 28: Lexer Improvements

**Goal:** Enhance the lexer for better Unicode support, f-string handling, and error recovery
**Estimated Effort:** 3-4 hours

---

## Overview

The lexer (`src/lexer/scanner.rs`) needs 5 improvements:
1. Unicode identifier support (currently ASCII-only)
2. F-string nested expression handling (braces in string literals)
3. Indentation misalignment detection (silent failures)
4. F-string escape sequences (missing `\0`, `\x`, `\u{}`)
5. Raw string delimiter counting (can't include backticks)

---

## Task 28.1: Unicode Identifier Support

**File:** `src/lexer/scanner.rs` lines 934-940

**Current Code:**
```rust
fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}
```

**Problem:** Only ASCII letters accepted - can't use `α`, `λ`, `件名`, etc.

**Fix:** Use Unicode-aware methods:
```rust
fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}
```

**Test Cases:**
```forma
# These should all be valid identifiers
f 计算(x: Int) -> Int = x * 2
let λ = |x| x + 1
let αβγ = 3.14159
```

---

## Task 28.2: Fix F-String Nested Expressions

**File:** `src/lexer/scanner.rs` lines 808-841

**Current Code:**
```rust
// Parse expression until }
let mut expr = String::new();
let mut brace_depth = 1;

loop {
    match self.peek() {
        // ... handles { and } for nesting
        Some(_) => {
            if let Some(c) = self.advance() {
                expr.push(c);
            }
        }
    }
}
```

**Problem:** When expression contains a string with braces like `f"{dict["key"]}"`, the `}` in the inner string terminates the expression early.

**Fix:** Track string parsing state:
```rust
// Parse expression until }
let mut expr = String::new();
let mut brace_depth = 1;
let mut in_string = false;
let mut string_char = '\0';

loop {
    match self.peek() {
        None | Some('\n') => {
            return self.error_token("unterminated expression in f-string");
        }
        Some('"') | Some('\'') if !in_string => {
            in_string = true;
            string_char = self.peek().unwrap();
            if let Some(c) = self.advance() {
                expr.push(c);
            }
        }
        Some(c) if in_string && c == string_char => {
            // Check if escaped
            let prev = expr.chars().last();
            if prev != Some('\\') {
                in_string = false;
            }
            if let Some(c) = self.advance() {
                expr.push(c);
            }
        }
        Some('}') if !in_string => {
            brace_depth -= 1;
            if brace_depth == 0 {
                self.advance();
                break;
            }
            if let Some(c) = self.advance() {
                expr.push(c);
            }
        }
        Some('{') if !in_string => {
            brace_depth += 1;
            if let Some(c) = self.advance() {
                expr.push(c);
            }
        }
        Some(_) => {
            if let Some(c) = self.advance() {
                expr.push(c);
            }
        }
    }
}
```

**Test Cases:**
```forma
# These should all work
let s = f"{map["key"]}"          # String inside expr
let t = f"{obj['name']}"         # Single quotes inside
let u = f"{x} + {map["a"]}"      # Multiple exprs
```

---

## Task 28.3: Fix Indentation Misalignment Detection

**File:** `src/lexer/scanner.rs` lines 315-330

**Current Code:**
```rust
} else if indent < current_indent {
    // Pop indent levels and count dedents
    while self.indent_stack.len() > 1 {
        let top = self.indent_stack.last().copied().unwrap_or(0);
        if indent >= top {
            break;
        }
        self.indent_stack.pop();
        self.pending_dedents += 1;
    }

    if self.pending_dedents > 0 {
        self.pending_dedents -= 1;
        return Some(self.make_token(TokenKind::Dedent));
    }
}
```

**Problem:** If dedent to 3 spaces but stack has [0, 4, 8], we pop down to [0, 4], then silently accept indent=3 even though it doesn't match any level.

**Fix:** After dedent loop, verify indent matches stack top:
```rust
} else if indent < current_indent {
    // Pop indent levels and count dedents
    while self.indent_stack.len() > 1 {
        let top = self.indent_stack.last().copied().unwrap_or(0);
        if indent >= top {
            break;
        }
        self.indent_stack.pop();
        self.pending_dedents += 1;
    }

    // Verify indent matches a level in the stack
    let final_top = self.indent_stack.last().copied().unwrap_or(0);
    if indent != final_top && self.pending_dedents > 0 {
        // Misaligned indentation
        return Some(self.error_token(format!(
            "inconsistent indentation: expected {} spaces but got {}",
            final_top, indent
        )));
    }

    if self.pending_dedents > 0 {
        self.pending_dedents -= 1;
        return Some(self.make_token(TokenKind::Dedent));
    }
}
```

**Test Cases:**
```forma
# This should error
f foo()
    if true
        x := 1
   y := 2       # Error: misaligned (3 spaces doesn't match 0 or 4)
```

---

## Task 28.4: Complete F-String Escape Sequences

**File:** `src/lexer/scanner.rs` lines 774-791

**Current Code:**
```rust
Some('\\') => {
    // Handle escape sequences
    self.advance();
    match self.advance() {
        Some('n') => current_text.push('\n'),
        Some('r') => current_text.push('\r'),
        Some('t') => current_text.push('\t'),
        Some('\\') => current_text.push('\\'),
        Some('"') => current_text.push('"'),
        Some('{') => current_text.push('{'),
        Some('}') => current_text.push('}'),
        Some(c) => {
            return self.error_token(format!("invalid escape in f-string: \\{}", c));
        }
        // ...
    }
}
```

**Problem:** Missing `\0` (null), `\x` (hex), and `\u{}` (unicode) escapes that regular strings support.

**Fix:** Add the missing cases to match regular string handling:
```rust
Some('\\') => {
    self.advance();
    match self.advance() {
        Some('n') => current_text.push('\n'),
        Some('r') => current_text.push('\r'),
        Some('t') => current_text.push('\t'),
        Some('\\') => current_text.push('\\'),
        Some('"') => current_text.push('"'),
        Some('{') => current_text.push('{'),
        Some('}') => current_text.push('}'),
        Some('0') => current_text.push('\0'),  // NEW: null
        Some('x') => {                          // NEW: hex escape
            if let Some(ch) = self.scan_hex_escape(2) {
                current_text.push(ch);
            } else {
                return self.error_token("invalid hex escape in f-string");
            }
        }
        Some('u') => {                          // NEW: unicode escape
            if !self.match_char('{') {
                return self.error_token("expected '{' in unicode escape");
            }
            if let Some(ch) = self.scan_unicode_escape() {
                current_text.push(ch);
            } else {
                return self.error_token("invalid unicode escape in f-string");
            }
        }
        Some(c) => {
            return self.error_token(format!("invalid escape in f-string: \\{}", c));
        }
        None => {
            return self.error_token("unterminated f-string");
        }
    }
}
```

**Test Cases:**
```forma
let s = f"null: \0"                  # Null character
let t = f"hex: \x41\x42\x43"         # ABC
let u = f"unicode: \u{1F600}"        # 😀
let v = f"mixed: {x}\u{2764}"        # Expression + unicode
```

---

## Task 28.5: Raw String Delimiter Counting

**File:** `src/lexer/scanner.rs` lines 421-447

**Current Code:**
```rust
fn scan_raw_string(&mut self) -> Token {
    let mut value = String::new();

    loop {
        match self.peek() {
            None => {
                return self.error_token("unterminated raw string");
            }
            Some('`') => {
                self.advance();
                break;
            }
            // ...
        }
    }

    self.make_token(TokenKind::String(value))
}
```

**Problem:** Can't include backticks in raw strings.

**Fix:** Support delimiter counting like Rust's raw strings:
```rust
// Called after initial ` is consumed
fn scan_raw_string(&mut self) -> Token {
    let mut value = String::new();

    // Count opening hashes (if any): r#`...`#
    let mut hash_count = 0;
    while self.peek() == Some('#') {
        self.advance();
        hash_count += 1;
    }

    // If we have hashes, we need the backtick
    if hash_count > 0 && self.peek() != Some('`') {
        return self.error_token("expected '`' after '#' in raw string");
    }
    if hash_count > 0 {
        self.advance(); // consume the second `
    }

    loop {
        match self.peek() {
            None => {
                return self.error_token("unterminated raw string");
            }
            Some('`') => {
                // Check for closing delimiter with correct hash count
                let pos = self.current;
                self.advance();

                let mut closing_hashes = 0;
                while closing_hashes < hash_count && self.peek() == Some('#') {
                    self.advance();
                    closing_hashes += 1;
                }

                if closing_hashes == hash_count {
                    break;
                } else {
                    // Not the real end, add the backtick and hashes to value
                    value.push('`');
                    for _ in 0..closing_hashes {
                        value.push('#');
                    }
                }
            }
            Some('\n') => {
                self.advance();
                self.line += 1;
                self.column = 1;
                value.push('\n');
            }
            Some(c) => {
                self.advance();
                value.push(c);
            }
        }
    }

    self.make_token(TokenKind::String(value))
}
```

**Note:** This requires updating how raw strings are initiated. The lexer must recognize `r#\`` as starting a delimited raw string. Update the main scan loop to handle this:

```rust
// In main scan_token, when seeing 'r':
'r' => {
    if self.peek() == Some('`') {
        self.advance(); // consume `
        return self.scan_raw_string();
    } else if self.peek() == Some('#') {
        // Delimited raw string: r#`...`#
        return self.scan_raw_string(); // scan_raw_string now handles #
    }
    self.scan_identifier()
}
```

**Test Cases:**
```forma
let a = r`simple raw string`
let b = r#`contains ` backtick`#
let c = r##`contains `# inside`##
let d = r`multi
line`
```

---

## Verification

After implementing all tasks:

1. Run the test suite:
```bash
cd forma && cargo test
```

2. Create test file `tests/lexer_sprint28.forma`:
```forma
# Sprint 28 Lexer Tests

# 28.1 Unicode identifiers
let 计数 = 0
let λ = |x| x + 1

# 28.2 F-string with nested braces
let map = { "key": "value" }
let s = f"Value: {map["key"]}"

# 28.3 Indentation (should work)
f test()
    if true
        x := 1
    y := 2  # Proper dedent to 4 spaces

# 28.4 F-string escapes
let t = f"null=\0, hex=\x41, unicode=\u{263A}"

# 28.5 Raw strings with backticks
let code = r#`let x = `hello``#

print("Lexer tests complete")
```

3. Run with interpreter:
```bash
cargo run -- run tests/lexer_sprint28.forma
```

---

## Summary

| Task | Description | File | Lines |
|------|-------------|------|-------|
| 28.1 | Unicode identifiers | scanner.rs | 934-940 |
| 28.2 | F-string nested expressions | scanner.rs | 808-841 |
| 28.3 | Indentation misalignment | scanner.rs | 315-330 |
| 28.4 | F-string escapes | scanner.rs | 774-791 |
| 28.5 | Raw string delimiters | scanner.rs | 421-447 |

**Dependencies:** None (standalone lexer changes)

---

## Claude Code Prompt

```
Sprint 28: Lexer Improvements for FORMA

Working directory: forma/

## Tasks

### 28.1: Unicode Identifier Support
File: src/lexer/scanner.rs (lines 934-940)
Change `c.is_ascii_alphabetic()` to `c.is_alphabetic()` and
`c.is_ascii_alphanumeric()` to `c.is_alphanumeric()` in
`is_ident_start()` and `is_ident_continue()`.

### 28.2: Fix F-String Nested Expressions
File: src/lexer/scanner.rs (lines 808-841)
When parsing expressions in f-strings, track whether we're inside
a string literal. Add `in_string` and `string_char` state variables.
Don't count braces that appear inside string literals.

### 28.3: Fix Indentation Misalignment Detection
File: src/lexer/scanner.rs (lines 315-330)
After the dedent loop, verify `indent == final_top`. If not equal
and we had pending dedents, emit an error token with message:
"inconsistent indentation: expected N spaces but got M"

### 28.4: Complete F-String Escape Sequences
File: src/lexer/scanner.rs (lines 774-791)
Add cases for:
- `\0` -> '\0' (null)
- `\x` -> call scan_hex_escape(2)
- `\u{` -> call scan_unicode_escape()
Match the existing escape handling in scan_string().

### 28.5: Raw String Delimiter Counting
File: src/lexer/scanner.rs (lines 421-447)
Support r#`...`# syntax:
1. After initial ` (or r), count # characters
2. For closing, require ` followed by same number of #
3. If counts don't match, include the ` and #s in the string value
4. Update raw string initiation in scan_token to handle r# prefix

## Testing
After changes:
1. cargo test
2. Create forma/tests/lexer_sprint28.forma with Unicode identifiers,
   f-strings with dict["key"], indentation tests, f-string escapes,
   and raw strings with backticks
3. cargo run -- run tests/lexer_sprint28.forma

All 251+ existing tests must continue to pass.
```
