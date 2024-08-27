//! This module defines the `Token` enum and associated methods,
//! used across the compiler.
//!
//! Tokens are first generated in the lexer, via reading a source file.
mod token_macros;

use crate::context::Span;

/// The `Token` struct represents a single token in the source code.
/// It contains the span of the token in the source file and the type of the token.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Token {
    pub(crate) token_type: TokenType,
    pub(crate) span: Span,
}

impl Token {
    /// Return a reference to the value of the token, if it is a literal or an identifier.
    pub(crate) fn literal_value(&self) -> Option<&String> {
        match &self.token_type {
            TokenType::Identifier(value) => Some(value),
            TokenType::String(value) => Some(value),
            TokenType::Integer(value) => Some(value),
            TokenType::Float(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TokenType {
    // Basic Tokens
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Comma,        // ,
    Dot,          // .
    Minus,        // -
    Plus,         // +
    Semicolon,    // ;
    Colon,        // :
    Slash,        // /
    Star,         // *
    Percent,      // %
    Ampersand,    // &
    Pipe,         // |
    Caret,        // ^
    Tilde,        // ~
    Question,     // ?
    At,           // @
    Underscore,   // _

    // Binary and Unary Expression Tokens
    Bang,               // !
    BangEqual,          // !=
    Equal,              // =
    EqualEqual,         // ==
    RightAngle,         // >
    GreaterEqual,       // >=
    LeftAngle,          // <
    LessEqual,          // <=
    ThinArrow,          // ->
    FatArrow,           // =>
    PlusEqual,          // +=
    MinusEqual,         // -=
    StarEqual,          // *=
    SlashEqual,         // /=
    PercentEqual,       // %=
    ColonColon,         // ::
    PipePipe,           // ||
    AmpersandAmpersand, // &&

    // Literals
    Identifier(String),
    String(String),
    Integer(String),
    Float(String),

    // Keywords
    Self_,      // self
    Enum,       // enum
    Error,      // error
    And,        // and
    Async,      // async
    Await,      // await
    Struct,     // struct
    Match,      // match
    Else,       // else
    Elif,       // elif
    False,      // false
    True,       // true
    Func,       // func
    For,        // for
    If,         // if
    Let,        // let
    Mut,        // mut
    Or,         // or
    Return,     // return
    While,      // while
    Use,        // use
    Extension,  // extension
    Implements, // implements
    Do,         // do
    In,         // in
    Eof,        // End of file
}
