//! Lexer/Scanner for the ARIA language.
//!
//! This module tokenizes ARIA source code into a stream of tokens,
//! handling indentation-significant syntax.

use crate::errors::LexError;
use crate::lexer::token::{Span, Token, TokenKind};

/// The lexer that tokenizes ARIA source code.
pub struct Scanner<'a> {
    source: &'a str,
    chars: std::iter::Peekable<std::str::CharIndices<'a>>,

    // Current position tracking
    start: usize,
    current: usize,
    line: usize,
    column: usize,
    start_column: usize,

    // Indentation tracking
    indent_stack: Vec<usize>,
    pending_dedents: usize,
    at_line_start: bool,

    // String interpolation support
    interpolation_depth: usize,
    brace_depth_stack: Vec<usize>,

    // Errors collected during scanning
    errors: Vec<LexError>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
            start: 0,
            current: 0,
            line: 1,
            column: 1,
            start_column: 1,
            indent_stack: vec![0],
            pending_dedents: 0,
            at_line_start: true,
            interpolation_depth: 0,
            brace_depth_stack: Vec::new(),
            errors: Vec::new(),
        }
    }

    /// Tokenize the entire source and return all tokens.
    pub fn scan_all(mut self) -> (Vec<Token>, Vec<LexError>) {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token();
            let is_eof = token.kind == TokenKind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }

        (tokens, self.errors)
    }

    /// Get the next token from the source.
    pub fn next_token(&mut self) -> Token {
        // Handle pending dedents first
        if self.pending_dedents > 0 {
            self.pending_dedents -= 1;
            return self.make_token(TokenKind::Dedent);
        }

        // Handle indentation at line start
        if self.at_line_start {
            if let Some(token) = self.handle_indentation() {
                return token;
            }
        }

        self.skip_whitespace_and_comments();

        self.start = self.current;
        self.start_column = self.column;

        let Some(c) = self.advance() else {
            // End of file - emit remaining dedents
            if self.indent_stack.len() > 1 {
                self.indent_stack.pop();
                self.pending_dedents = self.indent_stack.len() - 1;
                self.indent_stack.clear();
                self.indent_stack.push(0);
                return self.make_token(TokenKind::Dedent);
            }
            return self.make_token(TokenKind::Eof);
        };

        match c {
            // Single-character tokens
            '(' => self.make_token(TokenKind::LParen),
            ')' => self.make_token(TokenKind::RParen),
            '[' => self.make_token(TokenKind::LBracket),
            ']' => self.make_token(TokenKind::RBracket),
            '{' => {
                if self.interpolation_depth > 0 {
                    if let Some(depth) = self.brace_depth_stack.last_mut() {
                        *depth += 1;
                    }
                }
                self.make_token(TokenKind::LBrace)
            }
            '}' => {
                if self.interpolation_depth > 0 {
                    if let Some(depth) = self.brace_depth_stack.last_mut() {
                        if *depth > 0 {
                            *depth -= 1;
                        }
                    }
                }
                self.make_token(TokenKind::RBrace)
            }
            ',' => self.make_token(TokenKind::Comma),
            ';' => self.make_token(TokenKind::Semicolon),
            '@' => self.make_token(TokenKind::At),
            '%' => self.make_token(TokenKind::Percent),
            '^' => self.make_token(TokenKind::Caret),

            // Two-character tokens
            '+' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::PlusEq)
                } else {
                    self.make_token(TokenKind::Plus)
                }
            }
            '-' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::MinusEq)
                } else if self.match_char('>') {
                    self.make_token(TokenKind::Arrow)
                } else {
                    self.make_token(TokenKind::Minus)
                }
            }
            '*' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::StarEq)
                } else {
                    self.make_token(TokenKind::Star)
                }
            }
            '/' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::SlashEq)
                } else {
                    self.make_token(TokenKind::Slash)
                }
            }
            '=' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::EqEq)
                } else if self.match_char('>') {
                    self.make_token(TokenKind::FatArrow)
                } else {
                    self.make_token(TokenKind::Eq)
                }
            }
            '!' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::BangEq)
                } else {
                    self.make_token(TokenKind::Bang)
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::LtEq)
                } else if self.match_char('<') {
                    self.make_token(TokenKind::LtLt)
                } else {
                    self.make_token(TokenKind::Lt)
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::GtEq)
                } else if self.match_char('>') {
                    self.make_token(TokenKind::GtGt)
                } else {
                    self.make_token(TokenKind::Gt)
                }
            }
            '&' => {
                if self.match_char('&') {
                    self.make_token(TokenKind::AmpAmp)
                } else {
                    self.make_token(TokenKind::Amp)
                }
            }
            '|' => {
                if self.match_char('|') {
                    self.make_token(TokenKind::PipePipe)
                } else {
                    self.make_token(TokenKind::Pipe)
                }
            }
            ':' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::ColonEq)
                } else if self.match_char(':') {
                    self.make_token(TokenKind::ColonColon)
                } else {
                    self.make_token(TokenKind::Colon)
                }
            }
            '?' => {
                if self.match_char('?') {
                    self.make_token(TokenKind::QuestionQuestion)
                } else {
                    self.make_token(TokenKind::Question)
                }
            }
            '.' => {
                if self.match_char('.') {
                    if self.match_char('=') {
                        self.make_token(TokenKind::DotDotEq)
                    } else {
                        self.make_token(TokenKind::DotDot)
                    }
                } else if self.peek().is_some_and(|c| c.is_ascii_digit()) {
                    // Could be a float starting with .
                    self.scan_number_after_dot()
                } else {
                    self.make_token(TokenKind::Dot)
                }
            }

            // Newline
            '\n' => {
                self.line += 1;
                self.column = 1;
                self.at_line_start = true;
                self.make_token(TokenKind::Newline)
            }

            // String literals
            '"' => self.scan_string(),
            '`' => self.scan_raw_string(),
            '\'' => self.scan_char(),

            // Numbers
            '0'..='9' => self.scan_number(),

            // Identifiers and keywords
            c if is_ident_start(c) => self.scan_identifier(),

            _ => self.error_token(format!("unexpected character: '{}'", c)),
        }
    }

    fn handle_indentation(&mut self) -> Option<Token> {
        // Count leading whitespace
        let mut indent = 0;
        while let Some(&(_, c)) = self.chars.peek() {
            match c {
                ' ' => {
                    indent += 1;
                    self.advance();
                }
                '\t' => {
                    indent += 4; // Treat tabs as 4 spaces
                    self.advance();
                }
                '\n' => {
                    // Blank line, skip it
                    self.advance();
                    self.line += 1;
                    self.column = 1;
                    indent = 0;
                    continue;
                }
                '#' => {
                    // Comment line, skip to end
                    self.skip_line_comment();
                    indent = 0;
                    continue;
                }
                _ => break,
            }
        }

        self.at_line_start = false;

        // Check if at EOF
        if self.chars.peek().is_none() {
            return None;
        }

        let current_indent = *self.indent_stack.last().unwrap();

        if indent > current_indent {
            self.indent_stack.push(indent);
            return Some(self.make_token(TokenKind::Indent));
        } else if indent < current_indent {
            // Pop indent levels and count dedents
            while self.indent_stack.len() > 1 {
                let top = *self.indent_stack.last().unwrap();
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

        None
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek() {
                Some(' ') | Some('\t') | Some('\r') => {
                    self.advance();
                }
                Some('#') => {
                    self.skip_line_comment();
                }
                _ => break,
            }
        }
    }

    fn skip_line_comment(&mut self) {
        while let Some(c) = self.peek() {
            if c == '\n' {
                break;
            }
            self.advance();
        }
    }

    fn scan_string(&mut self) -> Token {
        let mut value = String::new();

        loop {
            match self.peek() {
                None | Some('\n') => {
                    return self.error_token("unterminated string");
                }
                Some('"') => {
                    self.advance();
                    break;
                }
                Some('\\') => {
                    self.advance();
                    match self.advance() {
                        Some('n') => value.push('\n'),
                        Some('r') => value.push('\r'),
                        Some('t') => value.push('\t'),
                        Some('\\') => value.push('\\'),
                        Some('\'') => value.push('\''),
                        Some('"') => value.push('"'),
                        Some('0') => value.push('\0'),
                        Some('x') => {
                            if let Some(ch) = self.scan_hex_escape(2) {
                                value.push(ch);
                            } else {
                                return self.error_token("invalid hex escape");
                            }
                        }
                        Some('u') => {
                            if !self.match_char('{') {
                                return self.error_token("expected '{' in unicode escape");
                            }
                            if let Some(ch) = self.scan_unicode_escape() {
                                value.push(ch);
                            } else {
                                return self.error_token("invalid unicode escape");
                            }
                        }
                        Some(c) => {
                            return self.error_token(format!("invalid escape sequence: \\{}", c));
                        }
                        None => {
                            return self.error_token("unterminated string");
                        }
                    }
                }
                Some('{') => {
                    // String interpolation - for now, just include the brace
                    // Full interpolation support will be handled in a future phase
                    self.advance();
                    value.push('{');
                }
                Some(c) => {
                    self.advance();
                    value.push(c);
                }
            }
        }

        self.make_token(TokenKind::String(value))
    }

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

    fn scan_char(&mut self) -> Token {
        let c = match self.peek() {
            None | Some('\'') | Some('\n') => {
                return self.error_token("empty character literal");
            }
            Some('\\') => {
                self.advance();
                match self.advance() {
                    Some('n') => '\n',
                    Some('r') => '\r',
                    Some('t') => '\t',
                    Some('\\') => '\\',
                    Some('\'') => '\'',
                    Some('"') => '"',
                    Some('0') => '\0',
                    Some('x') => match self.scan_hex_escape(2) {
                        Some(ch) => ch,
                        None => return self.error_token("invalid hex escape in char"),
                    },
                    Some('u') => {
                        if !self.match_char('{') {
                            return self.error_token("expected '{' in unicode escape");
                        }
                        match self.scan_unicode_escape() {
                            Some(ch) => ch,
                            None => return self.error_token("invalid unicode escape in char"),
                        }
                    }
                    Some(c) => {
                        return self.error_token(format!("invalid escape sequence: \\{}", c));
                    }
                    None => return self.error_token("unterminated character literal"),
                }
            }
            Some(c) => {
                self.advance();
                c
            }
        };

        if !self.match_char('\'') {
            return self.error_token("unterminated character literal");
        }

        self.make_token(TokenKind::Char(c))
    }

    fn scan_hex_escape(&mut self, count: usize) -> Option<char> {
        let mut value = 0u32;
        for _ in 0..count {
            let digit = self.peek()?.to_digit(16)?;
            self.advance();
            value = value * 16 + digit;
        }
        char::from_u32(value)
    }

    fn scan_unicode_escape(&mut self) -> Option<char> {
        let mut value = 0u32;
        let mut digits = 0;

        while let Some(c) = self.peek() {
            if c == '}' {
                self.advance();
                break;
            }
            let digit = c.to_digit(16)?;
            self.advance();
            value = value * 16 + digit;
            digits += 1;
            if digits > 6 {
                return None;
            }
        }

        if digits == 0 {
            return None;
        }

        char::from_u32(value)
    }

    fn scan_number(&mut self) -> Token {
        // Check for hex, binary, or octal
        if self.peek_prev() == Some('0') {
            match self.peek() {
                Some('x') | Some('X') => {
                    self.advance();
                    return self.scan_hex_number();
                }
                Some('b') | Some('B') => {
                    self.advance();
                    return self.scan_binary_number();
                }
                Some('o') | Some('O') => {
                    self.advance();
                    return self.scan_octal_number();
                }
                _ => {}
            }
        }

        // Decimal number
        self.scan_decimal_digits();

        // Check for float
        let is_float = if self.peek() == Some('.') && self.peek_next() != Some('.') {
            // Look ahead to make sure it's not a range operator
            if let Some(c) = self.peek_next() {
                if c.is_ascii_digit() {
                    self.advance(); // consume '.'
                    self.scan_decimal_digits();
                    true
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        };

        // Check for exponent
        let has_exponent = if self.peek() == Some('e') || self.peek() == Some('E') {
            self.advance();
            if self.peek() == Some('+') || self.peek() == Some('-') {
                self.advance();
            }
            self.scan_decimal_digits();
            true
        } else {
            false
        };

        // Check for suffix
        if is_float || has_exponent {
            // Capture the numeric portion before consuming suffix
            let num_end = self.current;
            // Float suffix
            if self.peek() == Some('f') {
                self.advance();
                if self.match_str("32") {
                    // f32
                } else if self.match_str("64") {
                    // f64
                }
            }
            let num_str: String = self.source[self.start..num_end]
                .chars()
                .filter(|c| *c != '_')
                .collect();
            match num_str.parse::<f64>() {
                Ok(n) => self.make_token(TokenKind::Float(n)),
                Err(_) => self.error_token("invalid float literal"),
            }
        } else {
            // Capture the numeric portion before consuming suffix
            let num_end = self.current;
            // Integer suffix
            if self.peek().is_some_and(|c| c == 'i' || c == 'u') {
                self.advance();
                let _ = self.match_str("8")
                    || self.match_str("16")
                    || self.match_str("32")
                    || self.match_str("64")
                    || self.match_str("128");
            }

            let num_str: String = self.source[self.start..num_end]
                .chars()
                .filter(|c| *c != '_')
                .collect();
            match num_str.parse::<i128>() {
                Ok(n) => self.make_token(TokenKind::Int(n)),
                Err(_) => self.error_token("invalid integer literal"),
            }
        }
    }

    fn scan_number_after_dot(&mut self) -> Token {
        // We've seen '.' and the next char is a digit
        self.scan_decimal_digits();

        // Check for exponent
        if self.peek() == Some('e') || self.peek() == Some('E') {
            self.advance();
            if self.peek() == Some('+') || self.peek() == Some('-') {
                self.advance();
            }
            self.scan_decimal_digits();
        }

        let lexeme = self.current_lexeme();
        let num_str: String = lexeme.chars().filter(|c| *c != '_').collect();

        match num_str.parse::<f64>() {
            Ok(n) => self.make_token(TokenKind::Float(n)),
            Err(_) => self.error_token("invalid float literal"),
        }
    }

    fn scan_hex_number(&mut self) -> Token {
        while self.peek().is_some_and(|c| c.is_ascii_hexdigit() || c == '_') {
            self.advance();
        }

        // Skip suffix for now
        if self.peek().is_some_and(|c| c == 'i' || c == 'u') {
            self.advance();
            let _ = self.match_str("8")
                || self.match_str("16")
                || self.match_str("32")
                || self.match_str("64")
                || self.match_str("128");
        }

        let lexeme = self.current_lexeme();
        // Remove '0x' prefix and underscores
        let hex_str: String = lexeme[2..].chars().filter(|c| *c != '_').collect();

        match i128::from_str_radix(&hex_str, 16) {
            Ok(n) => self.make_token(TokenKind::Int(n)),
            Err(_) => self.error_token("invalid hex literal"),
        }
    }

    fn scan_binary_number(&mut self) -> Token {
        while self.peek().is_some_and(|c| c == '0' || c == '1' || c == '_') {
            self.advance();
        }

        if self.peek().is_some_and(|c| c == 'i' || c == 'u') {
            self.advance();
            let _ = self.match_str("8")
                || self.match_str("16")
                || self.match_str("32")
                || self.match_str("64")
                || self.match_str("128");
        }

        let lexeme = self.current_lexeme();
        let bin_str: String = lexeme[2..].chars().filter(|c| *c != '_').collect();

        match i128::from_str_radix(&bin_str, 2) {
            Ok(n) => self.make_token(TokenKind::Int(n)),
            Err(_) => self.error_token("invalid binary literal"),
        }
    }

    fn scan_octal_number(&mut self) -> Token {
        while self.peek().is_some_and(|c| ('0'..='7').contains(&c) || c == '_') {
            self.advance();
        }

        if self.peek().is_some_and(|c| c == 'i' || c == 'u') {
            self.advance();
            let _ = self.match_str("8")
                || self.match_str("16")
                || self.match_str("32")
                || self.match_str("64")
                || self.match_str("128");
        }

        let lexeme = self.current_lexeme();
        let oct_str: String = lexeme[2..].chars().filter(|c| *c != '_').collect();

        match i128::from_str_radix(&oct_str, 8) {
            Ok(n) => self.make_token(TokenKind::Int(n)),
            Err(_) => self.error_token("invalid octal literal"),
        }
    }

    fn scan_decimal_digits(&mut self) {
        while self.peek().is_some_and(|c| c.is_ascii_digit() || c == '_') {
            self.advance();
        }
    }

    fn scan_identifier(&mut self) -> Token {
        while self.peek().is_some_and(|c| is_ident_continue(c)) {
            self.advance();
        }

        let lexeme = self.current_lexeme();

        // Check if it's a keyword
        if let Some(kind) = TokenKind::keyword(lexeme) {
            self.make_token(kind)
        } else {
            self.make_token(TokenKind::Ident(lexeme.to_string()))
        }
    }

    // Helper methods

    fn advance(&mut self) -> Option<char> {
        if let Some((pos, c)) = self.chars.next() {
            self.current = pos + c.len_utf8();
            self.column += 1;
            Some(c)
        } else {
            None
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }

    fn peek_prev(&self) -> Option<char> {
        if self.start < self.current {
            self.source[self.start..self.current].chars().last()
        } else {
            None
        }
    }

    fn peek_next(&self) -> Option<char> {
        let mut chars = self.source[self.current..].chars();
        chars.next();
        chars.next()
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_str(&mut self, expected: &str) -> bool {
        let remaining = &self.source[self.current..];
        if remaining.starts_with(expected) {
            for _ in expected.chars() {
                self.advance();
            }
            true
        } else {
            false
        }
    }

    fn current_lexeme(&self) -> &str {
        &self.source[self.start..self.current]
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        Token::new(
            kind,
            Span::new(self.start, self.current, self.line, self.start_column),
            self.current_lexeme(),
        )
    }

    fn error_token(&mut self, message: impl Into<String>) -> Token {
        let msg = message.into();
        let span = Span::new(self.start, self.current, self.line, self.start_column);
        self.errors.push(LexError::new(msg.clone(), span));
        Token::new(TokenKind::Error(msg), span, self.current_lexeme())
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scan(source: &str) -> Vec<TokenKind> {
        let scanner = Scanner::new(source);
        let (tokens, _) = scanner.scan_all();
        tokens.into_iter().map(|t| t.kind).collect()
    }

    #[test]
    fn test_keywords() {
        assert_eq!(scan("f"), vec![TokenKind::F, TokenKind::Eof]);
        assert_eq!(scan("s"), vec![TokenKind::S, TokenKind::Eof]);
        assert_eq!(scan("if"), vec![TokenKind::If, TokenKind::Eof]);
        assert_eq!(scan("then"), vec![TokenKind::Then, TokenKind::Eof]);
        assert_eq!(scan("else"), vec![TokenKind::Else, TokenKind::Eof]);
    }

    #[test]
    fn test_operators() {
        assert_eq!(scan("+"), vec![TokenKind::Plus, TokenKind::Eof]);
        assert_eq!(scan("->"), vec![TokenKind::Arrow, TokenKind::Eof]);
        assert_eq!(scan("=="), vec![TokenKind::EqEq, TokenKind::Eof]);
        assert_eq!(scan(":="), vec![TokenKind::ColonEq, TokenKind::Eof]);
        assert_eq!(scan(".."), vec![TokenKind::DotDot, TokenKind::Eof]);
        assert_eq!(scan("..="), vec![TokenKind::DotDotEq, TokenKind::Eof]);
    }

    #[test]
    fn test_integers() {
        assert_eq!(scan("42"), vec![TokenKind::Int(42), TokenKind::Eof]);
        assert_eq!(scan("1_000"), vec![TokenKind::Int(1000), TokenKind::Eof]);
        assert_eq!(scan("0xFF"), vec![TokenKind::Int(255), TokenKind::Eof]);
        assert_eq!(scan("0b1010"), vec![TokenKind::Int(10), TokenKind::Eof]);
        assert_eq!(scan("0o777"), vec![TokenKind::Int(511), TokenKind::Eof]);
    }

    #[test]
    fn test_floats() {
        assert_eq!(scan("3.14"), vec![TokenKind::Float(3.14), TokenKind::Eof]);
        assert_eq!(scan("2.5e10"), vec![TokenKind::Float(2.5e10), TokenKind::Eof]);
    }

    #[test]
    fn test_strings() {
        assert_eq!(
            scan("\"hello\""),
            vec![TokenKind::String("hello".to_string()), TokenKind::Eof]
        );
        assert_eq!(
            scan("\"a\\nb\""),
            vec![TokenKind::String("a\nb".to_string()), TokenKind::Eof]
        );
    }

    #[test]
    fn test_chars() {
        assert_eq!(scan("'a'"), vec![TokenKind::Char('a'), TokenKind::Eof]);
        assert_eq!(scan("'\\n'"), vec![TokenKind::Char('\n'), TokenKind::Eof]);
    }

    #[test]
    fn test_identifiers() {
        assert_eq!(
            scan("foo"),
            vec![TokenKind::Ident("foo".to_string()), TokenKind::Eof]
        );
        assert_eq!(
            scan("_bar"),
            vec![TokenKind::Ident("_bar".to_string()), TokenKind::Eof]
        );
    }

    #[test]
    fn test_booleans() {
        assert_eq!(scan("T"), vec![TokenKind::True, TokenKind::Eof]);
        assert_eq!(scan("F"), vec![TokenKind::False, TokenKind::Eof]);
        assert_eq!(scan("true"), vec![TokenKind::True, TokenKind::Eof]);
        assert_eq!(scan("false"), vec![TokenKind::False, TokenKind::Eof]);
    }

    #[test]
    fn test_none() {
        assert_eq!(scan("N"), vec![TokenKind::None, TokenKind::Eof]);
        assert_eq!(scan("none"), vec![TokenKind::None, TokenKind::Eof]);
    }

    #[test]
    fn test_comments() {
        assert_eq!(
            scan("x # comment\ny"),
            vec![
                TokenKind::Ident("x".to_string()),
                TokenKind::Newline,
                TokenKind::Ident("y".to_string()),
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_indentation() {
        let source = "f foo\n    x = 1\n    y = 2";
        let tokens = scan(source);
        assert!(tokens.contains(&TokenKind::Indent));
    }

    #[test]
    fn test_dedentation() {
        let source = "f foo\n    x = 1\ny = 2";
        let tokens = scan(source);
        assert!(tokens.contains(&TokenKind::Dedent));
    }
}
