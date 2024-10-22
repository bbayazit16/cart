/// Returns the value of a token, if the value exists.
#[macro_export]
macro_rules! token_value {
    ($ident:expr) => {
        match $ident.token_type {
            $crate::token::TokenType::Identifier(ref s) => s.to_string(),
            $crate::token::TokenType::String(ref s) => s.to_string(),
            $crate::token::TokenType::Integer(ref s) => s.to_string(),
            $crate::token::TokenType::Float(ref s) => s.to_string(),
            $crate::token::TokenType::True => "true".to_string(),
            $crate::token::TokenType::False => "false".to_string(),
            _ => unreachable!(),
        }
    };
    ($ident:expr, $ty:ident) => {
        match $ident.token_type {
            $crate::token::TokenType::$ty(ref s) => s.to_string(),
            _ => panic!("Must be type {}", stringify!($ty)),
        }
    };
}
