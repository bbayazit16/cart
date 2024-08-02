use std::fmt;
macro_rules! define_tokens {
    ( $( $name:ident $( ( $( $type:ty ),* ) )? ),* $(,)? ) => {
        #[derive(Debug, Clone)]
        #[allow(dead_code)]
        pub enum Token {
            $(
                $name $( ( $( $type ),* ) )?,
            )*
        }

        impl fmt::Display for Token {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{:?}", self)
            }
        }

        // impl fmt::Debug for Token {
        //     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        //         fmt::Display::fmt(self, f)
        //     }
        // }
    }
}

define_tokens!(
    // Single-character tokens
    LeftParen(usize, usize, usize),      // (
    RightParen(usize, usize, usize),     // )
    LeftBrace(usize, usize, usize),      // {
    RightBrace(usize, usize, usize),     // }
    LeftBracket(usize, usize, usize),    // [
    RightBracket(usize, usize, usize),   // ]
    Comma(usize, usize, usize),          // ,
    Dot(usize, usize, usize),            // .
    Minus(usize, usize, usize),          // -
    Plus(usize, usize, usize),           // +
    Semicolon(usize, usize, usize),      // ;
    Colon(usize, usize, usize),          // :
    Slash(usize, usize, usize),          // /
    Star(usize, usize, usize),           // *
    Percent(usize, usize, usize),        // %
    Ampersand(usize, usize, usize),      // &
    Pipe(usize, usize, usize),           // |
    Caret(usize, usize, usize),          // ^
    Tilde(usize, usize, usize),          // ~
    Question(usize, usize, usize),       // ?
    At(usize, usize, usize),             // @
    Underscore(usize, usize, usize),     // _

    // One or two character tokens
    Bang(usize, usize, usize),               // !
    BangEqual(usize, usize, usize),          // !=
    Equal(usize, usize, usize),              // =
    EqualEqual(usize, usize, usize),         // ==
    Greater(usize, usize, usize),            // >
    GreaterEqual(usize, usize, usize),       // >=
    Less(usize, usize, usize),               // <
    LessEqual(usize, usize, usize),          // <=
    ThinArrow(usize, usize, usize),          // ->
    FatArrow(usize, usize, usize),           // =>
    PlusEqual(usize, usize, usize),          // +=
    MinusEqual(usize, usize, usize),         // -=
    StarEqual(usize, usize, usize),          // *=
    SlashEqual(usize, usize, usize),         // /=
    PercentEqual(usize, usize, usize),       // %=
    ColonColon(usize, usize, usize),         // ::
    PipePipe(usize, usize, usize),           // ||
    AmpersandAmpersand(usize, usize, usize), // &&

    // Literals
    Identifier(usize, usize, usize, String),
    String(usize, usize, usize, String),
    Number(usize, usize, usize, String, bool),

    // Keywords
    And(usize, usize, usize),            // and
    Async(usize, usize, usize),          // async
    Await(usize, usize, usize),          // await
    Struct(usize, usize, usize),         // struct
    Match(usize, usize, usize),          // match
    Else(usize, usize, usize),           // else
    False(usize, usize, usize),          // false
    True(usize, usize, usize),           // true
    Func(usize, usize, usize),           // func
    For(usize, usize, usize),            // for
    If(usize, usize, usize),             // if
    Let(usize, usize, usize),            // let
    Mut(usize, usize, usize),            // mut
    Or(usize, usize, usize),             // or
    Print(usize, usize, usize),          // print
    Return(usize, usize, usize),         // return
    While(usize, usize, usize),          // while
    Use(usize, usize, usize),            // use
    Extension(usize, usize, usize),      // extension
    Implements(usize, usize, usize),     // implements
    Require(usize, usize, usize),        // require
    Do(usize, usize, usize),             // do
    Eof(usize, usize, usize)             // End of file
);
