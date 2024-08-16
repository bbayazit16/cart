mod errors;
mod help;

use crate::context::FilePointer;
pub use errors::*;
pub use help::Help;

impl SyntaxError {
    pub fn file_position(&self) -> FilePointer {
        match self {
            SyntaxError::UnterminatedStringLiteral {
                file_pointer: file_position,
            }
            | SyntaxError::UnterminatedComment {
                file_pointer: file_position,
            }
            | SyntaxError::UnexpectedCharacter {
                file_pointer: file_position,
                ..
            }
            | SyntaxError::ExpectedDifferentCharacter {
                file_pointer: file_position,
                ..
            }
            | SyntaxError::InvalidNumberLiteral {
                file_pointer: file_position,
                ..
            } => *file_position,
        }
    }
}

impl TypeError {
    pub fn file_position(&self) -> FilePointer {
        match self {
            TypeError::IncorrectType {
                file_pointer: file_position,
                ..
            } => *file_position,
        }
    }
}
