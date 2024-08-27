//! Lexer module defines the `Lexer` struct and its associated methods used for
//! lexical analysis of source code.
//!
//! Lexer struct is responsible for taking in a file as an input, and outputting
//! the tokens of source code.
mod alpha;
mod common;
mod recovery;

use crate::context::{FileContext, Position};
use crate::errors::CompileError;
use crate::token::Token;

// Maximum buffer size for the lexer
const BUFFER_CAPACITY: usize = 2048;

/// The `Lexer` struct reads characters from a buffered file reader, processes them,
/// and produces tokens, instances of [`Token`].
///
/// The lexer does not read and return all tokens at once, and maintains a buffer
/// with capacity `BUFFER_CAPACITY`. The lexer maintains [`FilePointer`] as an
/// internal state, and the lexer can be recovered to a certain position by providing
/// a [`FilePointer`] struct or a [`Token`].
///
/// To report the lexer uses a `Reporter` instance to report errors that might occur
/// during lexing.
///
/// To use the lexer, create an instance of `Lexer` with a `BufReader<File>` and
/// a `Reporter`. Then, repeatedly call the `request_next_token` method to obtain
/// tokens until an EOF token is returned.
///
/// - `buffer`: A string buffer for holding the currently read segment of the
///   input.
/// - `context`: The file context for the Lexer. It also holds the reporter,
///   which is for logging and reporting errors encountered.
/// - `file_position`: The current position in the input file.
/// - `buffer_position`: The current position within the buffer.
/// - `buffer_start`: The starting index of the buffer. This changes when the
///   Lexer is reverted to a position.
/// - `buffer_eof`: A flag indicating whether the end of the input file has been
///   reached.
#[derive(Debug)]
pub(crate) struct Lexer {
    buffer: String,
    context: FileContext,
    position: Position,
    buffer_position: usize,
    buffer_start: usize,
    buffer_eof: bool,
}

impl Lexer {
    /// Given a [`FileContext`], initialize a new Lexer.
    pub(crate) fn new(context: FileContext) -> Lexer {
        Lexer {
            buffer: String::with_capacity(BUFFER_CAPACITY),
            context,
            position: Position::default(),
            buffer_position: 0,
            buffer_start: 0,
            buffer_eof: false,
        }
    }

    /// Advances the lexer, and returns a Result with either the next token,
    /// or a compile error.
    pub(crate) fn request_next_token(&mut self) -> Token {
        self.next_alpha()
    }

    /// Shorthand to report error, which delegates to
    /// the reporter field.
    pub(crate) fn report_error(&mut self, error: &CompileError) {
        self.context.reporter().report(error);
    }

    /// Returns true if the lexer is at end, false otherwise.
    pub(crate) fn is_at_end(&self) -> bool {
        self.buffer_eof && self.buffer_position >= self.buffer.len()
    }
}
