//! Errors module defines all errors that may occur when compiling the program.
//! 
//! Each error related to user fault uses [`FilePointer`] struct to report
//! the error, and provide help in that file position, if any.
//! 
use thiserror::Error;
use crate::context::FilePointer;

#[derive(Error, Debug)]
pub enum SyntaxError {
    #[error("unterminated string literal at {file_pointer}")]
    UnterminatedStringLiteral {
        file_pointer: FilePointer
    },

    #[error("unterminated comment at {file_pointer}")]
    UnterminatedComment {
        file_pointer: FilePointer
    },

    #[error("unexpected character '{character}' at {file_pointer}")]
    UnexpectedCharacter {
        file_pointer: FilePointer,
        character: char,
    },

    #[error("expected '{expected}' at {file_pointer}")]
    ExpectedDifferentCharacter {
        file_pointer: FilePointer,
        expected: String,
    },

    #[error("invalid number literal '{literal}' at {file_pointer}")]
    InvalidNumberLiteral {
        file_pointer: FilePointer,
        literal: String,
    },
}

#[derive(Error, Debug)]
pub enum TypeError {
    #[error("incorrect type '{incorrect}', expected '{expected}' at {file_pointer}")]
    IncorrectType {
        file_pointer: FilePointer,
        incorrect: String,
        expected: String,
    }
}

#[derive(Error, Debug)]
pub enum ResolutionError {
    #[error("undefined variable '{variable}' at {file_pointer}")]
    UndefinedVariable {
        file_pointer: FilePointer,
        variable: String,
    }
}

#[derive(Error, Debug)]
pub enum CompileError {
    #[error(transparent)]
    Syntax(#[from] SyntaxError),
    
    #[error(transparent)]
    TypeError(#[from] TypeError),

    #[error(transparent)]
    IO(#[from] std::io::Error),
}
