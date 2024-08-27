use crate::errors::{SyntaxError, TypeError};

/// The `help` trait is implemented for certain error types. If the `help` trait
/// is implemented, then the reporter will accordingly print the help messages.
pub(crate) trait Help {
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

impl Help for TypeError {
    fn help_message(&self) -> Option<String> {
        match self {
            TypeError::IndexingNonArray { .. } => {
                Some("Only array types can be indexed with []".into())
            }
            TypeError::IncorrectStructLiteral { .. } => {
                Some("Fill in the remaining fields of the struct".into())
            }
            TypeError::ImmutableAssignment { variable, .. } => Some(format!(
                "Mark {} as mutable using the `mut` keyword",
                variable
            )),
            _ => None,
        }
    }
}
