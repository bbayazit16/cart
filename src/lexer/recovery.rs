use crate::context::FilePointer;
use crate::errors::CompileError;
use crate::lexer::Lexer;
use crate::token::Token;
use std::io::{Seek, SeekFrom};

impl Lexer {
    /// Reverts the Lexer to the given `FilePointer`.
    /// This function is useful for recovering from errors. Since each token
    /// contains an instance of `FilePointer`, it is also possible to restore
    /// the Lexer position to a certain token.
    pub(crate) fn revert_to_position(&mut self, position: FilePointer) -> Result<(), CompileError> {
        self.file_pointer = position;

        // If the current position is **within** the buffer:
        if self.file_pointer.file_position >= self.buffer_start
            && self.file_pointer.file_position < self.buffer_start + self.buffer.len()
        {
            self.buffer_position = self.file_pointer.file_position - self.buffer_start;
        } else {
            // The current position is **not within** the buffer!:
            self.buffer.clear();
            self.buffer_start = self.file_pointer.file_position;
            self.buffer_position = 0;
            self.buffer_eof = false;
            self.context
                .reader()
                .seek(SeekFrom::Start(self.file_pointer.file_position as u64))?;
            self.refill_buffer()?;
        }

        Ok(())
    }

    /// Revert to file pointer and attempt to lex an identifier.
    /// Useful for error recovery.
    pub(super) fn revert_lex_identifier(
        &mut self,
        file_pointer: FilePointer,
        else_char: char,
    ) -> Token {
        match self.revert_to_position(file_pointer) {
            Err(ref e) => {
                self.report_error(e);
                Token::Eof(file_pointer)
            }
            Ok(_) => self.identifier_or_keyword(else_char),
        }
    }

    /// Try and recover a number or an identifier if an error has occurred.
    pub(super) fn recover_potential_number(
        &mut self,
        line_start: usize,
        is_float: bool,
        c: char,
    ) -> Token {
        let start = self.file_pointer;
        if c.is_ascii_digit() {
            while let Some(next) = self.peek() {
                if next.is_whitespace() || !next.is_alphanumeric() {
                    break;
                }
                self.advance();
            }
            let end = self.file_pointer.file_position;
            let text = self.unchecked_index_into_buffer(start.file_position, end);
            let all_ascii_digit = text.chars().all(|c| c.is_ascii_digit());
            if all_ascii_digit {
                Token::Number(
                    FilePointer {
                        file_position: start.file_position,
                        line_position: line_start,
                        ..self.file_pointer
                    },
                    text.into(),
                    is_float,
                )
            } else {
                self.revert_lex_identifier(start, c)
            }
        } else {
            self.revert_lex_identifier(self.file_pointer, c)
        }
    }
}
