use std::fmt;
macro_rules! define_tokens {
    ( $( $name:ident $( ( $( $type:ty ),* ) )? ),* $(,)? ) => {
        #[derive(Debug, Clone)]
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
    LeftParen(usize, usize),      // (
    RightParen(usize, usize),     // )
    LeftBrace(usize, usize),      // {
    RightBrace(usize, usize),     // }
    LeftBracket(usize, usize),    // [
    RightBracket(usize, usize),   // ]
    Comma(usize, usize),          // ,
    Dot(usize, usize),            // .
    Minus(usize, usize),          // -
    Plus(usize, usize),           // +
    Semicolon(usize, usize),      // ;
    Colon(usize, usize),          // :
    Slash(usize, usize),          // /
    Star(usize, usize),           // *
    Percent(usize, usize),        // %
    Ampersand(usize, usize),      // &
    Pipe(usize, usize),           // |
    Caret(usize, usize),          // ^
    Tilde(usize, usize),          // ~
    Question(usize, usize),       // ?
    At(usize, usize),             // @

    // One or two character tokens
    Bang(usize, usize),           // !
    BangEqual(usize, usize),      // !=
    Equal(usize, usize),          // =
    EqualEqual(usize, usize),     // ==
    Greater(usize, usize),        // >
    GreaterEqual(usize, usize),   // >=
    Less(usize, usize),           // <
    LessEqual(usize, usize),      // <=
    ThinArrow(usize, usize),      // ->
    FatArrow(usize, usize),       // =>
    PlusEqual(usize, usize),      // +=
    MinusEqual(usize, usize),     // -=
    StarEqual(usize, usize),      // *=
    SlashEqual(usize, usize),     // /=
    PercentEqual(usize, usize),   // %=
    AmpersandAmpersand(usize, usize), // &&
    PipePipe(usize, usize),       // ||

    // Literals
    Identifier(usize, usize, String),
    String(usize, usize, String),
    Number(usize, usize, String), // To handle both integers and floats

    // Keywords
    And(usize, usize),            // and
    Async(usize, usize),          // async
    Await(usize, usize),          // await
    Struct(usize, usize),         // struct
    Match(usize, usize),          // match
    Else(usize, usize),           // else
    False(usize, usize),          // false
    True(usize, usize),           // true
    Func(usize, usize),           // func
    For(usize, usize),            // for
    If(usize, usize),             // if
    Let(usize, usize),            // let
    Mut(usize, usize),            // mut
    Or(usize, usize),             // or
    Print(usize, usize),          // print
    Return(usize, usize),         // return
    While(usize, usize),          // while
    Use(usize, usize),            // use
    Extension(usize, usize),      // extension
    Implements(usize, usize),     // implements
    Require(usize, usize),        // require
    Do(usize, usize),             // do
    EOF(usize, usize)             // End of file
);
