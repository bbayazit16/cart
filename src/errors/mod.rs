//! Errors module defines all errors that may occur when compiling the program.
//!
//! Each error related to user fault uses [`FilePointer`] struct to report
//! the error, and provide help in that file position, if any.
mod help;
mod position;

pub(crate) use help::Help;

use crate::context::Span;
use thiserror::Error;

#[derive(Error, Debug)]
pub(crate) enum SyntaxError {
    #[error("unterminated string literal")]
    UnterminatedStringLiteral { span: Span },

    #[error("unterminated comment")]
    UnterminatedComment { span: Span },

    #[error("unexpected character '{character}'")]
    UnexpectedCharacter { span: Span, character: char },

    #[error("expected '{expected}'")]
    ExpectedDifferent {
        span: Span,
        expected: String,
    },

    #[error("invalid number literal '{literal}'")]
    InvalidNumberLiteral { span: Span, literal: String },
}

#[derive(Error, Debug)]
pub(crate) enum TypeError {
    #[error("found type '{incorrect}', expected '{expected}'")]
    IncorrectType {
        span: Span,
        incorrect: String,
        expected: String,
    },

    #[error("undefined variable '{variable}'")]
    UndefinedVariable { span: Span, variable: String },

    #[error("undefined function '{function}'")]
    UndefinedFunction { span: Span, function: String },

    #[error("attempted to index type '{found_type}'")]
    IndexingNonArray { span: Span, found_type: String },

    #[error("binary operation '{op}' between '{left}' and '{right}' are incompatible")]
    BinaryOpError {
        span: Span,
        op: String,
        left: String,
        right: String,
    },

    #[error("unit type `()` assignments to variables are forbidden")]
    UnitAssignment { span: Span },
    
    #[error("incorrect struct literal")]
    IncorrectStructLiteral { span: Span },
    
    #[error("unknown struct field '{field}' at struct '{struct_name}'")]
    UnknownStructField { span: Span, field: String, struct_name: String },
    
    #[error("assignment to immutable variable '{variable}'")]
    ImmutableAssignment { span: Span, variable: String },
}

#[derive(Error, Debug)]
pub(crate) enum CompileError {
    #[error(transparent)]
    Syntax(#[from] SyntaxError),

    #[error(transparent)]
    TypeError(#[from] TypeError),

    #[error(transparent)]
    IO(#[from] std::io::Error),
}
