use crate::error::LexerError;
use crate::token::Token;
use crate::token::Token::Identifier;
use std::fs::File;
use std::io::{BufReader, Read};

const BUFFER_CAPACITY: usize = 2024;

pub struct Lexer {
    reader: BufReader<File>,
    buffer: String,
    line: usize,
    file_position: usize,
    buffer_position: usize,
    buffer_eof: bool,
}

impl Lexer {
    pub fn new(reader: BufReader<File>) -> Lexer {
        Lexer {
            reader,
            buffer: String::with_capacity(BUFFER_CAPACITY),
            line: 1,
            file_position: 0,
            buffer_position: 0,
            buffer_eof: false,
        }
    }

    pub fn request_next_token(&mut self) -> Result<Token, LexerError> {
        match self.advance() {
            Some(current_char) => {
                match current_char {
                    
                    _ => todo!(), 
                }
            }
            None => Ok(Token::EOF(self.file_position, self.line)),
        }
    }

    pub fn is_at_end(&self) -> bool {
        self.buffer_position >= self.buffer.len() && self.buffer_eof
    }

    fn refill_buffer(&mut self) -> Result<(), LexerError> {
        if self.buffer_eof {
            return Ok(());
        }

        let mut temp_buffer = vec![0; BUFFER_CAPACITY];
        let bytes_read = self.reader.read(&mut temp_buffer)?;

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

    fn advance(&mut self) -> Option<char> {
        if let Some(value) = self.buf_check() {
            return value;
        }

        let ch = self.buffer[self.buffer_position..].chars().next().unwrap();
        self.buffer_position += ch.len_utf8();
        self.file_position += ch.len_utf8();
        Some(ch)
    }

    fn match_next(&mut self, other: char) -> bool {
        match self.peek() {
            Some(next) if next == other => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn peek(&mut self) -> Option<char> {
        if let Some(value) = self.buf_check() {
            return value;
        }

        self.buffer[self.buffer_position..].chars().next()
    }

    fn buf_check(&mut self) -> Option<Option<char>> {
        if self.buffer_position >= self.buffer.len() {
            if self.buffer_eof {
                return Some(None);
            } else {
                self.refill_buffer().unwrap();
                if self.buffer_position >= self.buffer.len() {
                    return Some(None);
                }
            }
        }
        None
    }
}
