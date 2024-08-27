use crate::context::Span;
use crate::errors::{SyntaxError, TypeError};

impl SyntaxError {
    /// Return the span of the error.
    pub(crate) fn span(&self) -> Span {
        match self {
            SyntaxError::UnterminatedStringLiteral { span }
            | SyntaxError::UnterminatedComment { span }
            | SyntaxError::UnexpectedCharacter { span, .. }
            | SyntaxError::ExpectedDifferent { span, .. }
            | SyntaxError::InvalidNumberLiteral { span, .. } => *span,
        }
    }
}

impl TypeError {
    /// Return the span of the error.
    pub(crate) fn span(&self) -> Span {
        match self {
            TypeError::IncorrectType { span, .. }
            | TypeError::UndefinedVariable { span, .. }
            | TypeError::UndefinedFunction { span, .. }
            | TypeError::IndexingNonArray { span, .. }
            | TypeError::BinaryOpError { span, .. }
            | TypeError::UnitAssignment { span }
            | TypeError::IncorrectStructLiteral { span, .. }
            | TypeError::UnknownStructField { span, .. }
            | TypeError::ImmutableAssignment { span, .. } => *span,
        }
    }
}
