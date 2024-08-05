use thiserror::Error;

#[derive(Error, Debug)]
pub enum SyntaxError {
    #[error("unterminated string literal at line {line}, position {line_position}")]
    UnterminatedStringLiteral {
        line: usize,
        file_position: usize,
        line_position: usize,
    },

    #[error("unterminated comment at line {line}, position {line_position}")]
    UnterminatedComment {
        line: usize,
        file_position: usize,
        line_position: usize,
    },

    #[error("unexpected character '{character}' at line {line}, position {line_position}")]
    UnexpectedCharacter {
        line: usize,
        file_position: usize,
        line_position: usize,
        character: char,
    },

    #[error("expected '{expected}' at line {line}, position {line_position}")]
    ExpectedDifferentCharacter {
        line: usize,
        file_position: usize,
        line_position: usize,
        expected: String,
    },

    #[error("invalid number literal '{literal}' at line {line}, position {line_position}")]
    InvalidNumberLiteral {
        line: usize,
        file_position: usize,
        line_position: usize,
        literal: String,
    },
}

#[derive(Error, Debug)]
pub enum CompileError {
    #[error(transparent)]
    Syntax(#[from] SyntaxError),

    #[error(transparent)]
    IO(#[from] std::io::Error),
}
