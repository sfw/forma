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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
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
    F, // function
    S, // struct
    E, // enum
    T, // trait / true shortcut (context-dependent)
    I, // impl
    M, // match

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
    None, // N or none

    // Built-in type constructors
    Some,
    Ok,
    Err,

    // Arithmetic operators
    Plus,    // +
    Minus,   // -
    Star,    // *
    Slash,   // /
    Percent, // %

    // Comparison operators
    EqEq,   // ==
    BangEq, // !=
    Lt,     // <
    LtEq,   // <=
    Gt,     // >
    GtEq,   // >=

    // Logical operators
    AmpAmp,   // &&
    PipePipe, // ||
    Bang,     // !

    // Bitwise operators
    Amp,   // &
    Pipe,  // |
    Caret, // ^
    LtLt,  // <<
    GtGt,  // >>

    // Assignment operators
    Eq,        // =
    ColonEq,   // :=
    PlusEq,    // +=
    MinusEq,   // -=
    StarEq,    // *=
    SlashEq,   // /=
    PercentEq, // %=

    // Special operators
    Question,         // ?
    QuestionQuestion, // ??
    Arrow,            // ->
    FatArrow,         // =>
    DotDot,           // ..
    DotDotEq,         // ..=
    ColonColon,       // ::
    Dot,              // .
    Comma,            // ,
    At,               // @

    // Delimiters
    LParen,    // (
    RParen,    // )
    LBracket,  // [
    RBracket,  // ]
    LBrace,    // {
    RBrace,    // }
    Colon,     // :
    Semicolon, // ;

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

/// Semantic identity of a source keyword, independent of its spelling.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Keyword {
    Function,
    Struct,
    Enum,
    Trait,
    Impl,
    Match,
    If,
    Then,
    Else,
    For,
    In,
    While,
    Loop,
    Break,
    Continue,
    Return,
    Async,
    Await,
    Spawn,
    Use,
    Module,
    Public,
    Mutable,
    Reference,
    Move,
    Unsafe,
    Type,
    Where,
    Linear,
    Affine,
}

/// Canonical spelling and accepted aliases for one keyword.
#[derive(Debug, Clone, Copy)]
pub struct KeywordSpec {
    pub keyword: Keyword,
    pub canonical: &'static str,
    pub aliases: &'static [&'static str],
    pub contextual: bool,
}

pub const KEYWORDS: &[KeywordSpec] = &[
    KeywordSpec {
        keyword: Keyword::Function,
        canonical: "f",
        aliases: &["function"],
        contextual: true,
    },
    KeywordSpec {
        keyword: Keyword::Struct,
        canonical: "s",
        aliases: &["struct"],
        contextual: true,
    },
    KeywordSpec {
        keyword: Keyword::Enum,
        canonical: "e",
        aliases: &["enum"],
        contextual: true,
    },
    KeywordSpec {
        keyword: Keyword::Trait,
        canonical: "t",
        aliases: &["trait"],
        contextual: true,
    },
    KeywordSpec {
        keyword: Keyword::Impl,
        canonical: "i",
        aliases: &["impl"],
        contextual: true,
    },
    KeywordSpec {
        keyword: Keyword::Match,
        canonical: "m",
        aliases: &["match"],
        contextual: true,
    },
    KeywordSpec {
        keyword: Keyword::If,
        canonical: "if",
        aliases: &[],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Then,
        canonical: "then",
        aliases: &[],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Else,
        canonical: "else",
        aliases: &[],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::For,
        canonical: "for",
        aliases: &[],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::In,
        canonical: "in",
        aliases: &[],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::While,
        canonical: "wh",
        aliases: &["while"],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Loop,
        canonical: "lp",
        aliases: &["loop"],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Break,
        canonical: "br",
        aliases: &["break"],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Continue,
        canonical: "ct",
        aliases: &["continue"],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Return,
        canonical: "ret",
        aliases: &["return"],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Async,
        canonical: "as",
        aliases: &["async"],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Await,
        canonical: "aw",
        aliases: &["await"],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Spawn,
        canonical: "sp",
        aliases: &["spawn"],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Use,
        canonical: "us",
        aliases: &["use"],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Module,
        canonical: "md",
        aliases: &["module", "mod"],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Public,
        canonical: "pub",
        aliases: &["public"],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Mutable,
        canonical: "mut",
        aliases: &[],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Reference,
        canonical: "ref",
        aliases: &[],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Move,
        canonical: "mv",
        aliases: &["move"],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Unsafe,
        canonical: "un",
        aliases: &["unsafe"],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Type,
        canonical: "type",
        aliases: &[],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Where,
        canonical: "where",
        aliases: &[],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Linear,
        canonical: "linear",
        aliases: &[],
        contextual: false,
    },
    KeywordSpec {
        keyword: Keyword::Affine,
        canonical: "affine",
        aliases: &[],
        contextual: false,
    },
];

impl Keyword {
    pub fn token(self) -> TokenKind {
        match self {
            Self::Function => TokenKind::F,
            Self::Struct => TokenKind::S,
            Self::Enum => TokenKind::E,
            Self::Trait => TokenKind::T,
            Self::Impl => TokenKind::I,
            Self::Match => TokenKind::M,
            Self::If => TokenKind::If,
            Self::Then => TokenKind::Then,
            Self::Else => TokenKind::Else,
            Self::For => TokenKind::For,
            Self::In => TokenKind::In,
            Self::While => TokenKind::Wh,
            Self::Loop => TokenKind::Lp,
            Self::Break => TokenKind::Br,
            Self::Continue => TokenKind::Ct,
            Self::Return => TokenKind::Ret,
            Self::Async => TokenKind::As,
            Self::Await => TokenKind::Aw,
            Self::Spawn => TokenKind::Sp,
            Self::Use => TokenKind::Us,
            Self::Module => TokenKind::Md,
            Self::Public => TokenKind::Pub,
            Self::Mutable => TokenKind::Mut,
            Self::Reference => TokenKind::Ref,
            Self::Move => TokenKind::Mv,
            Self::Unsafe => TokenKind::Un,
            Self::Type => TokenKind::Type,
            Self::Where => TokenKind::Where,
            Self::Linear => TokenKind::Linear,
            Self::Affine => TokenKind::Affine,
        }
    }
}

impl TokenKind {
    /// Returns the keyword for a given string, if it is a keyword.
    pub fn keyword(s: &str) -> Option<TokenKind> {
        if let Some(spec) = KEYWORDS
            .iter()
            .find(|spec| spec.canonical == s || spec.aliases.contains(&s))
        {
            return Some(spec.keyword.token());
        }
        match s {
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
