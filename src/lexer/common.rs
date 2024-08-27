use crate::context::Span;
use crate::errors::{CompileError, SyntaxError};
use crate::lexer::{Lexer, BUFFER_CAPACITY};
use std::io::Read;

impl Lexer {
    /// Advances the lexer while the current char
    /// is alphanumeric.
    pub(super) fn advance_while_alphanumeric(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Indexes the buffer as [start, end).
    /// This method is preferred over `self.buffer` as it also takes starting point
    /// into account.
    pub(super) fn unchecked_index_into_buffer(&self, start: usize, end: usize) -> &str {
        let buffer_start = start - self.buffer_start;
        let buffer_end = end - self.buffer_start;

        &self.buffer[buffer_start..buffer_end]
    }

    /// Advances the lexer by one position, returning the consumed char.
    pub(super) fn advance(&mut self) -> Option<char> {
        if self.buffer_eof && self.buffer_position >= self.buffer.len() {
            return None;
        }

        if let Err(e) = self.refill_buffer() {
            self.report_error(&e);
            return None;
        }

        let ch = self.buffer[self.buffer_position - self.buffer_start..]
            .chars()
            .next()?;
        self.buffer_position += ch.len_utf8();
        self.position.offset += ch.len_utf8();
        self.position.column += ch.len_utf8();
        Some(ch)
    }

    /// Looks ahead to the next token without advancing the token.
    pub(super) fn peek(&mut self) -> Option<char> {
        if self.buffer_eof && self.buffer_position >= self.buffer.len() {
            return None;
        }

        if let Err(e) = self.refill_buffer() {
            self.report_error(&e);
            return None;
        }

        self.buffer[self.buffer_position - self.buffer_start..]
            .chars()
            .next()
    }

    /// Matches the next character in the lexer.
    /// If the char matches, advances the lexer and returns true.
    pub(super) fn match_next(&mut self, other: char) -> bool {
        match self.peek() {
            Some(next) if next == other => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    /// Matches the second next character in the lexer.
    /// If the char matches, advances the lexer and returns true.
    pub(super) fn match_next_next(&mut self, other: char) -> bool {
        if self.buffer_eof && self.buffer_position >= self.buffer.len() {
            return false;
        }

        if let Err(e) = self.refill_buffer() {
            self.report_error(&e);
            return false;
        }

        let mut chars = self.buffer[self.buffer_position - self.buffer_start..].chars();
        chars.next();
        match chars.next() {
            Some(next) if next == other => {
                self.advance();
                self.advance();
                true
            }
            _ => false,
        }
    }

    /// Returns true if the number's base prefix can be
    /// parsed by the compiler. This includes hexadecimal,
    /// octal, and binary.
    pub(super) fn match_base_prefix(&mut self) -> bool {
        match self.peek() {
            Some('x') | Some('o') | Some('b') => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    /// Skips whitespaces and comments, adjusting the
    /// file pointer and refilling the buffer, if necessary.
    pub(super) fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek() {
                Some(' ') | Some('\r') | Some('\t') => {
                    self.advance();
                }
                Some('\n') => {
                    self.advance();
                    self.position.line += 1;
                    self.position.column = 1;
                }
                Some('/') => {
                    if self.match_next_next('/') {
                        while let Some(c) = self.peek() {
                            if c == '\n' {
                                break;
                            }
                            self.advance();
                        }
                    } else if self.match_next_next('*') {
                        let mut was_broken = false;
                        while let Some(c) = self.advance() {
                            if c == '*' {
                                if self.match_next('/') {
                                    was_broken = true;
                                    break;
                                }
                            } else if c == '\n' {
                                self.position.line += 1;
                                self.position.column = 1;
                            }
                        }
                        if self.peek().is_none() && !was_broken {
                            self.report_error(&CompileError::Syntax(
                                SyntaxError::UnterminatedComment {
                                    span: Span::single(self.position),
                                },
                            ));
                        }
                    } else {
                        // Single '/'
                        return;
                    }
                }
                _ => return,
            }
        }
    }

    /// Refills the Lexer's buffer.
    ///
    /// Returns Ok(()) if no errors have occurred, and CompileError
    /// with an IO Error otherwise.
    pub(super) fn refill_buffer(&mut self) -> Result<(), CompileError> {
        if self.buffer_eof {
            return Ok(());
        }

        let mut temp_buffer = vec![0; BUFFER_CAPACITY];
        let bytes_read = self.context.reader().read(&mut temp_buffer)?;
        if bytes_read == 0 {
            self.buffer_eof = true;
        } else {
            let temp_str = String::from_utf8_lossy(&temp_buffer[..bytes_read]);
            if self.buffer_position < self.buffer.len() {
                self.buffer.drain(..self.buffer_position);
            } else {
                self.buffer.clear();
            }
            self.buffer.push_str(&temp_str);
            self.buffer_position = 0;
            if bytes_read < BUFFER_CAPACITY {
                self.buffer_eof = true;
            }
        }

        Ok(())
    }
}
