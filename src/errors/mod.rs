//! Errors module defines all errors that may occur when compiling the program.
//!
//! Each error related to user fault uses [`FilePointer`] struct to report
//! the error, and provide help in that file position, if any.
mod help;
mod position;

pub(crate) use help::Help;
pub(crate) use position::Position;

use crate::context::FilePointer;
use thiserror::Error;

#[derive(Error, Debug)]
pub(crate) enum SyntaxError {
    #[error("unterminated string literal at {file_pointer}")]
    UnterminatedStringLiteral { file_pointer: FilePointer },

    #[error("unterminated comment at {file_pointer}")]
    UnterminatedComment { file_pointer: FilePointer },

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
pub(crate) enum TypeError {
    #[error("found type '{incorrect}', expected '{expected}' at {file_pointer}")]
    IncorrectType {
        file_pointer: FilePointer,
        incorrect: String,
        expected: String,
    },

    #[error("undefined variable '{variable}' at {file_pointer}")]
    UndefinedVariable {
        file_pointer: FilePointer,
        variable: String,
    },
    
    #[error("undefined function '{function}' at {file_pointer}")]
    UndefinedFunction {
        file_pointer: FilePointer,
        function: String,
    },

    #[error("attempted to index type '{found_type}' at {file_pointer}")]
    IndexingNonArray {
        file_pointer: FilePointer,
        found_type: String,
    },

    #[error("binary operation '{op}' between '{left}' and '{right}' at {file_pointer} are incompatible")]
    BinaryOpError {
        op: String,
        left: String,
        right: String,
        file_pointer: FilePointer,
    },
    
    #[error("unit type `()` assignments to variables are forbidden. Assignment found at {file_pointer}")]
    UnitAssignment {
        file_pointer: FilePointer,
    },
}

// #[derive(Error, Debug)]
// #[allow(unused)]
// pub(crate) enum ResolutionError {
// }

#[derive(Error, Debug)]
pub(crate) enum CompileError {
    #[error(transparent)]
    Syntax(#[from] SyntaxError),

    #[error(transparent)]
    TypeError(#[from] TypeError),

    #[error(transparent)]
    IO(#[from] std::io::Error),
}
