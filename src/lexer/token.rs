//! Token definitions for the FORMA lexer.
//!
//! This module defines all tokens that can appear in FORMA source code,
//! including keywords, operators, literals, and delimiters.

use std::fmt;

/// Part of an f-string: either literal text or an expression to interpolate.
#[derive(Debug, Clone, PartialEq)]
pub enum FStringPart {
    /// Literal text between interpolations
    Text(String),
    /// Expression to interpolate (the source code between { and })
    Expr(String),
}

/// A token with its location in the source file.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub lexeme: String,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span, lexeme: impl Into<String>) -> Self {
        Self {
            kind,
            span,
            lexeme: lexeme.into(),
        }
    }

    pub fn is(&self, kind: TokenKind) -> bool {
        self.kind == kind
    }
}

/// Source location information.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

impl Span {
    pub fn new(start: usize, end: usize, line: usize, column: usize) -> Self {
        Self {
            start,
            end,
            line,
            column,
        }
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            line: self.line.min(other.line),
            column: if self.line <= other.line {
                self.column
            } else {
                other.column
            },
        }
    }
}

/// All token types in the FORMA language.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Keywords (single character)
    F,      // function
    S,      // struct
    E,      // enum
    T,      // trait / true shortcut (context-dependent)
    I,      // impl
    M,      // match

    // Keywords (multi-character)
    If,
    Then,
    Else,
    For,
    In,
    Wh,     // while
    Lp,     // loop
    Br,     // break
    Ct,     // continue
    Ret,    // return
    As,     // async
    Aw,     // await
    Sp,     // spawn
    Us,     // use
    Md,     // module
    Pub,    // public
    Mut,    // mutable
    Ref,    // reference parameter
    Mv,     // move
    Un,     // unsafe
    Type,   // type alias
    Where,  // where clause
    Linear, // linear type qualifier
    Affine, // affine type qualifier

    // Boolean/None literals (can also be keywords)
    True,
    False,
    None,   // N or none

    // Built-in type constructors
    Some,
    Ok,
    Err,

    // Arithmetic operators
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Percent,    // %

    // Comparison operators
    EqEq,       // ==
    BangEq,     // !=
    Lt,         // <
    LtEq,       // <=
    Gt,         // >
    GtEq,       // >=

    // Logical operators
    AmpAmp,     // &&
    PipePipe,   // ||
    Bang,       // !

    // Bitwise operators
    Amp,        // &
    Pipe,       // |
    Caret,      // ^
    LtLt,       // <<
    GtGt,       // >>

    // Assignment operators
    Eq,         // =
    ColonEq,    // :=
    PlusEq,     // +=
    MinusEq,    // -=
    StarEq,     // *=
    SlashEq,    // /=
    PercentEq,  // %=

    // Special operators
    Question,       // ?
    QuestionQuestion, // ??
    Arrow,          // ->
    FatArrow,       // =>
    DotDot,         // ..
    DotDotEq,       // ..=
    ColonColon,     // ::
    Dot,            // .
    Comma,          // ,
    At,             // @

    // Delimiters
    LParen,     // (
    RParen,     // )
    LBracket,   // [
    RBracket,   // ]
    LBrace,     // {
    RBrace,     // }
    Colon,      // :
    Semicolon,  // ;

    // Literals
    Int(i128),
    Float(f64),
    String(String),
    Char(char),
    /// Interpolated string (f-string) with parts: either literal text or expressions to interpolate
    FString(Vec<FStringPart>),

    // Identifiers
    Ident(String),

    // Indentation tokens
    Newline,
    Indent,
    Dedent,

    // Special
    Eof,
    Error(String),
}

impl TokenKind {
    /// Returns the keyword for a given string, if it is a keyword.
    pub fn keyword(s: &str) -> Option<TokenKind> {
        match s {
            // Single-character keywords
            "f" => Some(TokenKind::F),
            "s" => Some(TokenKind::S),
            "e" => Some(TokenKind::E),
            "t" => Some(TokenKind::T),
            "i" => Some(TokenKind::I),
            "m" => Some(TokenKind::M),

            // Multi-character keywords
            "if" => Some(TokenKind::If),
            "then" => Some(TokenKind::Then),
            "else" => Some(TokenKind::Else),
            "for" => Some(TokenKind::For),
            "in" => Some(TokenKind::In),
            "wh" => Some(TokenKind::Wh),
            "lp" => Some(TokenKind::Lp),
            "br" => Some(TokenKind::Br),
            "ct" => Some(TokenKind::Ct),
            "ret" => Some(TokenKind::Ret),
            "return" => Some(TokenKind::Ret),
            "as" => Some(TokenKind::As),
            "aw" => Some(TokenKind::Aw),
            "sp" => Some(TokenKind::Sp),
            "us" => Some(TokenKind::Us),
            "md" => Some(TokenKind::Md),
            "pub" => Some(TokenKind::Pub),
            "mut" => Some(TokenKind::Mut),
            "ref" => Some(TokenKind::Ref),
            "mv" => Some(TokenKind::Mv),
            "un" => Some(TokenKind::Un),
            "type" => Some(TokenKind::Type),
            "where" => Some(TokenKind::Where),

            "linear" => Some(TokenKind::Linear),
            "affine" => Some(TokenKind::Affine),

            // Boolean literals
            "T" => Some(TokenKind::True),
            "F" => Some(TokenKind::False),
            "true" => Some(TokenKind::True),
            "false" => Some(TokenKind::False),

            // None literal
            "N" => Some(TokenKind::None),
            "none" => Some(TokenKind::None),

            // Built-in constructors
            "Some" => Some(TokenKind::Some),
            "Ok" => Some(TokenKind::Ok),
            "Err" => Some(TokenKind::Err),
            "ok" => Some(TokenKind::Ok),
            "err" => Some(TokenKind::Err),

            _ => Option::None,
        }
    }

    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            TokenKind::F
                | TokenKind::S
                | TokenKind::E
                | TokenKind::T
                | TokenKind::I
                | TokenKind::M
                | TokenKind::If
                | TokenKind::Then
                | TokenKind::Else
                | TokenKind::For
                | TokenKind::In
                | TokenKind::Wh
                | TokenKind::Lp
                | TokenKind::Br
                | TokenKind::Ct
                | TokenKind::Ret
                | TokenKind::As
                | TokenKind::Aw
                | TokenKind::Sp
                | TokenKind::Us
                | TokenKind::Md
                | TokenKind::Pub
                | TokenKind::Mut
                | TokenKind::Ref
                | TokenKind::Mv
                | TokenKind::Un
                | TokenKind::Type
                | TokenKind::Where
                | TokenKind::Linear
                | TokenKind::Affine
                | TokenKind::True
                | TokenKind::False
                | TokenKind::None
                | TokenKind::Some
                | TokenKind::Ok
                | TokenKind::Err
        )
    }

    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            TokenKind::Int(_)
                | TokenKind::Float(_)
                | TokenKind::String(_)
                | TokenKind::Char(_)
                | TokenKind::FString(_)
                | TokenKind::True
                | TokenKind::False
                | TokenKind::None
        )
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::F => write!(f, "f"),
            TokenKind::S => write!(f, "s"),
            TokenKind::E => write!(f, "e"),
            TokenKind::T => write!(f, "t"),
            TokenKind::I => write!(f, "i"),
            TokenKind::M => write!(f, "m"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Then => write!(f, "then"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::For => write!(f, "for"),
            TokenKind::In => write!(f, "in"),
            TokenKind::Wh => write!(f, "wh"),
            TokenKind::Lp => write!(f, "lp"),
            TokenKind::Br => write!(f, "br"),
            TokenKind::Ct => write!(f, "ct"),
            TokenKind::Ret => write!(f, "ret"),
            TokenKind::As => write!(f, "as"),
            TokenKind::Aw => write!(f, "aw"),
            TokenKind::Sp => write!(f, "sp"),
            TokenKind::Us => write!(f, "us"),
            TokenKind::Md => write!(f, "md"),
            TokenKind::Pub => write!(f, "pub"),
            TokenKind::Mut => write!(f, "mut"),
            TokenKind::Ref => write!(f, "ref"),
            TokenKind::Mv => write!(f, "mv"),
            TokenKind::Un => write!(f, "un"),
            TokenKind::Type => write!(f, "type"),
            TokenKind::Where => write!(f, "where"),
            TokenKind::Linear => write!(f, "linear"),
            TokenKind::Affine => write!(f, "affine"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::None => write!(f, "none"),
            TokenKind::Some => write!(f, "Some"),
            TokenKind::Ok => write!(f, "Ok"),
            TokenKind::Err => write!(f, "Err"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::EqEq => write!(f, "=="),
            TokenKind::BangEq => write!(f, "!="),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::LtEq => write!(f, "<="),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::GtEq => write!(f, ">="),
            TokenKind::AmpAmp => write!(f, "&&"),
            TokenKind::PipePipe => write!(f, "||"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::Amp => write!(f, "&"),
            TokenKind::Pipe => write!(f, "|"),
            TokenKind::Caret => write!(f, "^"),
            TokenKind::LtLt => write!(f, "<<"),
            TokenKind::GtGt => write!(f, ">>"),
            TokenKind::Eq => write!(f, "="),
            TokenKind::ColonEq => write!(f, ":="),
            TokenKind::PlusEq => write!(f, "+="),
            TokenKind::MinusEq => write!(f, "-="),
            TokenKind::StarEq => write!(f, "*="),
            TokenKind::SlashEq => write!(f, "/="),
            TokenKind::PercentEq => write!(f, "%="),
            TokenKind::Question => write!(f, "?"),
            TokenKind::QuestionQuestion => write!(f, "??"),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::FatArrow => write!(f, "=>"),
            TokenKind::DotDot => write!(f, ".."),
            TokenKind::DotDotEq => write!(f, "..="),
            TokenKind::ColonColon => write!(f, "::"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Comma => write!(f, ","),
            TokenKind::At => write!(f, "@"),
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LBracket => write!(f, "["),
            TokenKind::RBracket => write!(f, "]"),
            TokenKind::LBrace => write!(f, "{{"),
            TokenKind::RBrace => write!(f, "}}"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Int(n) => write!(f, "{}", n),
            TokenKind::Float(n) => write!(f, "{}", n),
            TokenKind::String(s) => write!(f, "\"{}\"", s),
            TokenKind::Char(c) => write!(f, "'{}'", c),
            TokenKind::FString(parts) => {
                write!(f, "f\"")?;
                for part in parts {
                    match part {
                        FStringPart::Text(s) => write!(f, "{}", s)?,
                        FStringPart::Expr(e) => write!(f, "{{{}}}", e)?,
                    }
                }
                write!(f, "\"")
            }
            TokenKind::Ident(s) => write!(f, "{}", s),
            TokenKind::Newline => write!(f, "NEWLINE"),
            TokenKind::Indent => write!(f, "INDENT"),
            TokenKind::Dedent => write!(f, "DEDENT"),
            TokenKind::Eof => write!(f, "EOF"),
            TokenKind::Error(msg) => write!(f, "ERROR: {}", msg),
        }
    }
}
