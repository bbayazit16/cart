//! The help module implements help messages for certain
//! error types. If the `help` trait is implemented, the
//! reporter will accordingly print help messages.

use crate::errors::SyntaxError;

pub trait Help {
    fn help_message(&self) -> Option<String>;
}

impl Help for SyntaxError {
    fn help_message(&self) -> Option<String> {
        match self {
            SyntaxError::UnterminatedStringLiteral { .. } => {
                Some("Close the string using '\"'".into())
            }
            SyntaxError::UnterminatedComment { .. } => {
                Some("Terminate the comment using */".into())
            }
            SyntaxError::InvalidNumberLiteral { .. } => {
                Some("Valid bases are: 16 ('x'), 10, 8 ('o'), 2 ('b')".into())
            }
            _ => None,
        }
    }
}
