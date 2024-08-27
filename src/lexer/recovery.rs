use crate::context::{Position, Span};
use crate::errors::CompileError;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::io::{Seek, SeekFrom};

impl Lexer {
    /// Reverts the Lexer to the given `Position`.
    /// This function is useful for recovering from errors. Since each token
    /// contains an instance of `Position`, it is also possible to restore
    /// the Lexer position to a certain token.
    pub(crate) fn revert_to_position(&mut self, position: Position) -> Result<(), CompileError> {
        self.position = position;

        // If the current position is **within** the buffer:
        if self.position.offset >= self.buffer_start
            && self.position.offset < self.buffer_start + self.buffer.len()
        {
            self.buffer_position = self.position.offset - self.buffer_start;
        } else {
            // The current position is **not within** the buffer!:
            self.buffer.clear();
            self.buffer_start = self.position.offset;
            self.buffer_position = 0;
            self.buffer_eof = false;
            self.context
                .reader()
                .seek(SeekFrom::Start(self.position.offset as u64))?;
            self.refill_buffer()?;
        }

        Ok(())
    }

    /// Revert to file pointer and attempt to lex an identifier.
    /// Useful for error recovery.
    pub(super) fn revert_lex_identifier(
        &mut self,
        file_pointer: Position,
        else_char: char,
    ) -> Token {
        match self.revert_to_position(file_pointer) {
            Err(ref e) => {
                self.report_error(e);
                Token {
                    span: Span::single(self.position),
                    token_type: TokenType::Error,
                }
            }
            Ok(_) => self.identifier_or_keyword(else_char),
        }
    }

    /// Try and recover a number or an identifier if an error has occurred.
    pub(super) fn recover_potential_number(&mut self, is_float: bool, c: char) -> Token {
        let start = self.position;
        if c.is_ascii_digit() {
            while let Some(next) = self.peek() {
                if next.is_whitespace() || !next.is_alphanumeric() {
                    break;
                }
                self.advance();
            }
            let end = self.position.offset;
            let text = self.unchecked_index_into_buffer(start.offset, end);
            let all_ascii_digit = text.chars().all(|c| c.is_ascii_digit());
            if all_ascii_digit {
                Token {
                    token_type: if is_float {
                        TokenType::Float(text.into())
                    } else {
                        TokenType::Integer(text.into())
                    },
                    span: Span::new(start, self.position),
                }
            } else {
                self.revert_lex_identifier(start, c)
            }
        } else {
            self.revert_lex_identifier(self.position, c)
        }
    }
}
