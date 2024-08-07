mod token_macros;
use crate::*;

define_tokens_with_display! {
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
    Number(String, bool),
    
    // Keywords
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
    Eof         // End of file
}
