use crate::context::FilePointer;
use crate::errors::{SyntaxError, TypeError};

pub(crate) trait Position {
    /// Return the file position of the error.
    fn file_position(&self) -> FilePointer;
}

impl Position for SyntaxError {
    fn file_position(&self) -> FilePointer {
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

impl Position for TypeError {
    fn file_position(&self) -> FilePointer {
        match self {
            TypeError::IncorrectType {
                file_pointer: file_position,
                ..
            } => *file_position,
        }
    }
}
