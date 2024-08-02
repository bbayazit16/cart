use crate::error::{LexerError, SyntaxError};
use crate::reporter::Reporter;
use crate::token::Token;
use std::fs::File;
use std::io::{BufReader, Read};

const BUFFER_CAPACITY: usize = 2024;

pub struct Lexer {
    reader: BufReader<File>,
    reporter: Reporter,
    buffer: String,
    line: usize,
    file_position: usize,
    line_position: usize,
    buffer_position: usize,
    buffer_eof: bool,
}

impl Lexer {
    pub fn new(reader: BufReader<File>, reporter: Reporter) -> Lexer {
        Lexer {
            reader,
            buffer: String::with_capacity(BUFFER_CAPACITY),
            reporter,
            line: 1,
            file_position: 0,
            line_position: 0,
            buffer_position: 0,
            buffer_eof: false,
        }
    }

    pub fn request_next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace_and_comments();
        match self.advance() {
            Some(current_char) => {
                let token = match current_char {
                    '(' => Token::LeftParen(self.file_position, self.line, self.line_position),
                    ')' => Token::RightParen(self.file_position, self.line, self.line_position),
                    '{' => Token::LeftBrace(self.file_position, self.line, self.line_position),
                    '}' => Token::RightBrace(self.file_position, self.line, self.line_position),
                    '[' => Token::LeftBracket(self.file_position, self.line, self.line_position),
                    ']' => Token::RightBracket(self.file_position, self.line, self.line_position),
                    ',' => Token::Comma(self.file_position, self.line, self.line_position),
                    '.' => Token::Dot(self.file_position, self.line, self.line_position),
                    '-' => {
                        if self.match_next('>') {
                            Token::ThinArrow(self.file_position, self.line, self.line_position)
                        } else if self.match_next('=') {
                            Token::MinusEqual(self.file_position, self.line, self.line_position)
                        } else {
                            Token::Minus(self.file_position, self.line, self.line_position)
                        }
                    }
                    '+' => {
                        if self.match_next('=') {
                            Token::PlusEqual(self.file_position, self.line, self.line_position)
                        } else {
                            Token::Plus(self.file_position, self.line, self.line_position)
                        }
                    }
                    ';' => Token::Semicolon(self.file_position, self.line, self.line_position),
                    ':' => {
                        if self.match_next(':') {
                            Token::ColonColon(self.file_position, self.line, self.line_position)
                        } else {
                            Token::Colon(self.file_position, self.line, self.line_position)
                        }
                    }
                    '/' => {
                        if self.match_next('=') {
                            Token::SlashEqual(self.file_position, self.line, self.line_position)
                        } else {
                            Token::Slash(self.file_position, self.line, self.line_position)
                        }
                    }
                    '*' => {
                        if self.match_next('=') {
                            Token::StarEqual(self.file_position, self.line, self.line_position)
                        } else {
                            Token::Star(self.file_position, self.line, self.line_position)
                        }
                    }
                    '%' => {
                        if self.match_next('=') {
                            Token::PercentEqual(self.file_position, self.line, self.line_position)
                        } else {
                            Token::Percent(self.file_position, self.line, self.line_position)
                        }
                    }
                    '&' => {
                        if self.match_next('&') {
                            Token::AmpersandAmpersand(
                                self.file_position,
                                self.line,
                                self.line_position,
                            )
                        } else {
                            Token::Ampersand(self.file_position, self.line, self.line_position)
                        }
                    }
                    '|' => {
                        if self.match_next('|') {
                            Token::PipePipe(self.file_position, self.line, self.line_position)
                        } else {
                            Token::Pipe(self.file_position, self.line, self.line_position)
                        }
                    }
                    '^' => Token::Caret(self.file_position, self.line, self.line_position),
                    '~' => Token::Tilde(self.file_position, self.line, self.line_position),
                    '?' => Token::Question(self.file_position, self.line, self.line_position),
                    '@' => Token::At(self.file_position, self.line, self.line_position),
                    '_' => Token::Underscore(self.file_position, self.line, self.line_position),
                    '!' => {
                        if self.match_next('=') {
                            Token::BangEqual(self.file_position, self.line, self.line_position)
                        } else {
                            Token::Bang(self.file_position, self.line, self.line_position)
                        }
                    }
                    '=' => {
                        if self.match_next('=') {
                            Token::EqualEqual(self.file_position, self.line, self.line_position)
                        } else if self.match_next('>') {
                            Token::FatArrow(self.file_position, self.line, self.line_position)
                        } else {
                            Token::Equal(self.file_position, self.line, self.line_position)
                        }
                    }
                    '>' => {
                        if self.match_next('=') {
                            Token::GreaterEqual(self.file_position, self.line, self.line_position)
                        } else {
                            Token::Greater(self.file_position, self.line, self.line_position)
                        }
                    }
                    '<' => {
                        if self.match_next('=') {
                            Token::LessEqual(self.file_position, self.line, self.line_position)
                        } else {
                            Token::Less(self.file_position, self.line, self.line_position)
                        }
                    }
                    _ => {
                        return if current_char.is_alphabetic() || current_char == '_' {
                            self.identifier_or_keyword(current_char)
                        } else if current_char.is_ascii_digit()
                            || (current_char == '0' && self.match_base_prefix())
                        {
                            self.number(current_char)
                        } else if current_char == '"' {
                            self.string()
                        } else {
                            let e = LexerError::Syntax(SyntaxError::UnexpectedCharacter {
                                line: self.line,
                                file_position: self.file_position,
                                line_position: self.line_position,
                                character: current_char,
                            });
                            self.reporter.report(&e);

                            Err(e)
                        }
                    }
                };
                Ok(token)
            }
            None => Ok(Token::Eof(
                self.file_position,
                self.line,
                self.line_position,
            )),
        }
    }

    fn identifier_or_keyword(&mut self, initial_char: char) -> Result<Token, LexerError> {
        let start = self.file_position - initial_char.len_utf8();
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }
        let end = self.file_position;
        let text = &self.buffer[start..end];
        let token = match text {
            "and" => Token::And(start, self.line, self.line_position),
            "async" => Token::Async(start, self.line, self.line_position),
            "await" => Token::Await(start, self.line, self.line_position),
            "struct" => Token::Struct(start, self.line, self.line_position),
            "match" => Token::Match(start, self.line, self.line_position),
            "else" => Token::Else(start, self.line, self.line_position),
            "false" => Token::False(start, self.line, self.line_position),
            "true" => Token::True(start, self.line, self.line_position),
            "func" => Token::Func(start, self.line, self.line_position),
            "for" => Token::For(start, self.line, self.line_position),
            "if" => Token::If(start, self.line, self.line_position),
            "let" => Token::Let(start, self.line, self.line_position),
            "mut" => Token::Mut(start, self.line, self.line_position),
            "or" => Token::Or(start, self.line, self.line_position),
            "print" => Token::Print(start, self.line, self.line_position),
            "return" => Token::Return(start, self.line, self.line_position),
            "while" => Token::While(start, self.line, self.line_position),
            "use" => Token::Use(start, self.line, self.line_position),
            "extension" => Token::Extension(start, self.line, self.line_position),
            "implements" => Token::Implements(start, self.line, self.line_position),
            "require" => Token::Require(start, self.line, self.line_position),
            "do" => Token::Do(start, self.line, self.line_position),
            _ => Token::Identifier(start, self.line, self.line_position, text.to_string()),
        };
        Ok(token)
    }

    fn number(&mut self, initial_char: char) -> Result<Token, LexerError> {
        let start = self.file_position - initial_char.len_utf8();
        let mut is_float = false;
        let mut base = 10;

        if initial_char == '0' {
            match self.peek() {
                Some('x') => {
                    base = 16;
                    self.advance();
                }
                Some('o') => {
                    base = 8;
                    self.advance();
                }
                Some('b') => {
                    base = 2;
                    self.advance();
                }
                Some(c) if c.is_ascii_digit() => {
                    let e = LexerError::Syntax(SyntaxError::InvalidNumberLiteral {
                        line: self.line,
                        file_position: self.file_position,
                        line_position: self.line_position,
                        literal: format!("0{}", c),
                    });
                    self.reporter.report(&e);
                    return Err(e);
                }
                Some(c) if c.is_alphabetic() => {
                    let e = LexerError::Syntax(SyntaxError::InvalidNumberLiteral {
                        line: self.line,
                        file_position: self.file_position,
                        line_position: self.line_position,
                        literal: format!("0{}", c),
                    });
                    self.reporter.report(&e);
                    return Err(e);
                }
                _ => {
                    // 0
                    return Ok(Token::Number(
                        start,
                        self.line,
                        self.line_position,
                        "0".to_string(),
                        false,
                    ));
                }
            }
        }

        let valid_chars = |c: char| c.is_digit(base);

        let mut has_digits = initial_char.is_digit(base);
        let mut last_char_underscore = false;

        while let Some(c) = self.peek() {
            if valid_chars(c) {
                has_digits = true;
                self.advance();
                last_char_underscore = false;
            } else if c == '_' {
                if last_char_underscore {
                    let e = LexerError::Syntax(SyntaxError::UnexpectedCharacter {
                        line: self.line,
                        file_position: self.file_position,
                        line_position: self.line_position,
                        character: c,
                    });

                    self.reporter.report(&e);
                    return Err(e);
                }
                last_char_underscore = true;
                self.advance();
            } else if c == '.' && base == 10 && !is_float {
                self.advance();
                if self.peek().map_or(false, |c| c.is_ascii_digit()) {
                    is_float = true;
                    has_digits = false;
                } else {
                    let e = LexerError::Syntax(SyntaxError::InvalidNumberLiteral {
                        line: self.line,
                        file_position: self.file_position,
                        line_position: self.line_position,
                        literal: self.buffer[start..self.file_position].to_string(),
                    });
                    self.reporter.report(&e);
                    return Err(e);
                }
            } else {
                break;
            }
        }

        if last_char_underscore || !has_digits {
            let e = LexerError::Syntax(SyntaxError::InvalidNumberLiteral {
                line: self.line,
                file_position: self.file_position,
                line_position: self.line_position,
                literal: self.buffer[start..self.file_position].to_string(),
            });

            self.reporter.report(&e);
            return Err(e);
        }

        let text = self.buffer[start..self.file_position].replace('_', "");
        Ok(Token::Number(
            start,
            self.line,
            self.line_position,
            text,
            is_float,
        ))
    }

    fn string(&mut self) -> Result<Token, LexerError> {
        let start = self.file_position;
        let start_line = self.line;
        let start_line_position = self.line_position;
        let mut value = String::new();

        while let Some(c) = self.advance() {
            if c == '"' {
                break;
            }
            if c == '\\' {
                match self.advance() {
                    Some('n') => value.push('\n'),
                    Some('t') => value.push('\t'),
                    Some('r') => value.push('\r'),
                    Some('\\') => value.push('\\'),
                    Some('\"') => value.push('\"'),
                    Some(c) => value.push(c),
                    None => {
                        let e = LexerError::Syntax(SyntaxError::UnterminatedStringLiteral {
                            line: start_line,
                            file_position: start,
                            line_position: start_line_position,
                        });

                        self.reporter.report(&e);
                        return Err(e);
                    }
                }
            } else {
                if c == '\n' {
                    self.line += 1;
                    self.line_position = 0;
                }
                value.push(c);
            }
        }
        if self.peek().is_none() && !self.buffer.ends_with('"') {
            let e = LexerError::Syntax(SyntaxError::UnterminatedStringLiteral {
                line: start_line,
                file_position: start,
                line_position: start_line_position,
            });

            self.reporter.report(&e);
            return Err(e);
        }
        Ok(Token::String(start, self.line, self.line_position, value))
    }

    pub fn is_at_end(&self) -> bool {
        self.buffer_eof && self.buffer_position >= self.buffer.len()
    }

    fn advance(&mut self) -> Option<char> {
        if self.buffer_eof && self.buffer_position >= self.buffer.len() {
            return None;
        }

        if let Err(e) = self.refill_buffer() {
            self.reporter.report(&e);
            return None;
        }

        let ch = self.buffer[self.buffer_position..].chars().next().unwrap();
        self.buffer_position += ch.len_utf8();
        self.file_position += ch.len_utf8();
        self.line_position += ch.len_utf8();
        Some(ch)
    }

    fn peek(&mut self) -> Option<char> {
        if self.buffer_eof && self.buffer_position >= self.buffer.len() {
            return None;
        }

        if let Err(e) = self.refill_buffer() {
            self.reporter.report(&e);
            return None;
        }

        self.buffer[self.buffer_position..].chars().next()
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

    fn match_base_prefix(&mut self) -> bool {
        match self.peek() {
            Some('x') | Some('o') | Some('b') => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            while let Some(c) = self.peek() {
                match c {
                    ' ' | '\r' | '\t' => {
                        self.advance();
                    }
                    '\n' => {
                        self.line += 1;
                        self.line_position = 0;
                        self.advance();
                    }
                    '/' => {
                        if self.match_next('/') {
                            while let Some(c) = self.advance() {
                                if c == '\n' {
                                    self.line += 1;
                                    self.line_position = 0;
                                    break;
                                }
                            }
                        } else if self.match_next('*') {
                            while let Some(c) = self.advance() {
                                if c == '*' && self.match_next('/') {
                                    break;
                                }
                                if c == '\n' {
                                    self.line += 1;
                                    self.line_position = 0;
                                }
                            }
                        } else {
                            return;
                        }
                    }
                    _ => return,
                }
            }
            if self.peek().is_none() {
                break;
            }
        }
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

    // fn buf_check(&mut self) -> Option<Option<char>> {
    //     if self.buffer_position >= self.buffer.len() {
    //         if self.buffer_eof {
    //             return Some(None);
    //         } else {
    //             self.refill_buffer().unwrap();
    //             if self.buffer_position >= self.buffer.len() {
    //                 return Some(None);
    //             }
    //         }
    //     }
    //     None
    // }
}
