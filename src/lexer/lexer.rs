//! Lexer module defines the `Lexer` struct and its associated methods used for
//! lexical analysis of source code.
//!
//! Lexer struct is responsible for taking in a file as an input, and outputting
//! the tokens of source code.
//!
use crate::context::{FileContext, FilePointer};
use crate::errors::{CompileError, SyntaxError};
use crate::token::Token;
use std::io::{Read, Seek, SeekFrom};

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
pub struct Lexer {
    buffer: String,
    context: FileContext,
    file_pointer: FilePointer,
    buffer_position: usize,
    buffer_start: usize,
    buffer_eof: bool,
}

impl Lexer {
    /// Given a [`FileContext`], initialize a new Lexer.
    pub fn new(context: FileContext) -> Lexer {
        Lexer {
            buffer: String::with_capacity(BUFFER_CAPACITY),
            context,
            file_pointer: FilePointer::default(),
            buffer_position: 0,
            buffer_start: 0,
            buffer_eof: false,
        }
    }

    /// Advances the lexer, and returns a Result with either the next token,
    /// or a compile error.
    ///
    /// Advances the buffer position, and reports errors if any has occurred
    /// during the lexing process.
    pub fn request_next_token(&mut self) -> Token {
        self.skip_whitespace_and_comments();
        let file_pointer = self.file_pointer;
        match self.advance() {
            Some(current_char) => {
                match current_char {
                    '(' => Token::LeftParen(file_pointer),
                    ')' => Token::RightParen(file_pointer),
                    '{' => Token::LeftBrace(file_pointer),
                    '}' => Token::RightBrace(file_pointer),
                    '[' => Token::LeftBracket(file_pointer),
                    ']' => Token::RightBracket(file_pointer),
                    ',' => Token::Comma(file_pointer),
                    '.' => Token::Dot(file_pointer),
                    '-' => {
                        if self.match_next('>') {
                            Token::ThinArrow(file_pointer)
                        } else if self.match_next('=') {
                            Token::MinusEqual(file_pointer)
                        } else {
                            Token::Minus(file_pointer)
                        }
                    }
                    '+' => {
                        if self.match_next('=') {
                            Token::PlusEqual(file_pointer)
                        } else {
                            Token::Plus(file_pointer)
                        }
                    }
                    ';' => Token::Semicolon(file_pointer),
                    ':' => {
                        if self.match_next(':') {
                            Token::ColonColon(file_pointer)
                        } else {
                            Token::Colon(file_pointer)
                        }
                    }
                    '/' => {
                        if self.match_next('=') {
                            Token::SlashEqual(file_pointer)
                        } else {
                            Token::Slash(file_pointer)
                        }
                    }
                    '*' => {
                        if self.match_next('=') {
                            Token::StarEqual(file_pointer)
                        } else {
                            Token::Star(file_pointer)
                        }
                    }
                    '%' => {
                        if self.match_next('=') {
                            Token::PercentEqual(file_pointer)
                        } else {
                            Token::Percent(file_pointer)
                        }
                    }
                    '&' => {
                        if self.match_next('&') {
                            Token::AmpersandAmpersand(file_pointer)
                        } else {
                            Token::Ampersand(file_pointer)
                        }
                    }
                    '|' => {
                        if self.match_next('|') {
                            Token::PipePipe(file_pointer)
                        } else {
                            Token::Pipe(file_pointer)
                        }
                    }
                    '^' => Token::Caret(file_pointer),
                    '~' => Token::Tilde(file_pointer),
                    '?' => Token::Question(file_pointer),
                    '@' => Token::At(file_pointer),
                    '!' => {
                        if self.match_next('=') {
                            Token::BangEqual(file_pointer)
                        } else {
                            Token::Bang(file_pointer)
                        }
                    }
                    '=' => {
                        if self.match_next('=') {
                            Token::EqualEqual(file_pointer)
                        } else if self.match_next('>') {
                            Token::FatArrow(file_pointer)
                        } else {
                            Token::Equal(file_pointer)
                        }
                    }
                    '>' => {
                        if self.match_next('=') {
                            Token::GreaterEqual(file_pointer)
                        } else {
                            Token::RightAngle(file_pointer)
                        }
                    }
                    '<' => {
                        if self.match_next('=') {
                            Token::LessEqual(file_pointer)
                        } else {
                            Token::LeftAngle(file_pointer)
                        }
                    }
                    _ => {
                        if current_char.is_alphabetic() || current_char == '_' {
                            self.identifier_or_keyword(current_char)
                        } else if current_char.is_ascii_digit()
                            || (current_char == '0' && self.match_base_prefix())
                        {
                            self.number(current_char)
                        } else if current_char == '"' {
                            self.string()
                        } else {
                            let e = CompileError::Syntax(SyntaxError::UnexpectedCharacter {
                                file_pointer,
                                character: current_char,
                            });
                            self.report_error(&e);

                            // Error recovery: attempt to parse as an identifier
                            self.identifier_or_keyword(current_char)
                        }
                    }
                }
            }
            None => Token::Eof(file_pointer),
        }
    }

    /// Reverts the Lexer to the given `FilePointer`.
    /// This function is useful for recovering from errors. Since each token
    /// contains an instance of `FilePointer`, it is also possible to restore
    /// the Lexer position to a certain token.
    pub fn revert_to_position(&mut self, position: FilePointer) -> Result<(), CompileError> {
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
    
    /// Return the current file pointer.
    pub fn file_pointer(&self) -> FilePointer {
        self.file_pointer
    }

    /// Lex an identifier or a keyword.
    fn identifier_or_keyword(&mut self, initial_char: char) -> Token {
        let start = self.file_pointer.file_position - initial_char.len_utf8();
        let starting_file_position = FilePointer {
            file_position: start,
            line: self.file_pointer.line,
            line_position: self.file_pointer.line_position - initial_char.len_utf8(),
        };

        self.advance_while_alphanumeric();

        let end = self.file_pointer.file_position;
        let text = self.unchecked_index_into_buffer(start, end);
        match text {
            "enum" => Token::Enum(starting_file_position),
            "error" => Token::Error(starting_file_position),
            "and" => Token::And(starting_file_position),
            "async" => Token::Async(starting_file_position),
            "await" => Token::Await(starting_file_position),
            "struct" => Token::Struct(starting_file_position),
            "match" => Token::Match(starting_file_position),
            "else" => Token::Else(starting_file_position),
            "elif" => Token::Elif(starting_file_position),
            "false" => Token::False(starting_file_position),
            "true" => Token::True(starting_file_position),
            "func" => Token::Func(starting_file_position),
            "for" => Token::For(starting_file_position),
            "if" => Token::If(starting_file_position),
            "let" => Token::Let(starting_file_position),
            "mut" => Token::Mut(starting_file_position),
            "or" => Token::Or(starting_file_position),
            "return" => Token::Return(starting_file_position),
            "while" => Token::While(starting_file_position),
            "use" => Token::Use(starting_file_position),
            "extension" => Token::Extension(starting_file_position),
            "implements" => Token::Implements(starting_file_position),
            "do" => Token::Do(starting_file_position),
            "in" => Token::In(starting_file_position),
            "_" => Token::Underscore(starting_file_position),
            _ => Token::Identifier(starting_file_position, text.to_string()),
        }
    }

    /// Lex a number. Return CompileError if any error has occurred.
    fn number(&mut self, initial_char: char) -> Token {
        let start = self.file_pointer.file_position - initial_char.len_utf8();
        let line_start = self.file_pointer.line_position - initial_char.len_utf8();

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
                Some(c) if c.is_ascii_digit() || c.is_alphabetic() => {
                    // Unknown base
                    let e = CompileError::Syntax(SyntaxError::InvalidNumberLiteral {
                        file_pointer: self.file_pointer,
                        literal: format!("0{}", c),
                    });
                    self.report_error(&e);

                    // Error recovery:

                    // Consume the unknown base
                    self.advance();
                    // Try parsing the number again
                    return self.recover_potential_number(line_start, is_float, c);
                }
                _ => {
                    // 0
                    return Token::Number(
                        FilePointer {
                            file_position: start,
                            line_position: line_start,
                            ..self.file_pointer
                        },
                        "0".to_string(),
                        false,
                    );
                }
            }
        }

        let valid_chars = |c: char| c.is_digit(base);

        let mut has_digits = initial_char != '0';
        let mut last_char_underscore = false;

        while let Some(c) = self.peek() {
            if valid_chars(c) {
                has_digits = true;
                self.advance();
                last_char_underscore = false;
            } else if c == '_' {
                if last_char_underscore {
                    // Two consecutive __ encountered
                    let e = CompileError::Syntax(SyntaxError::UnexpectedCharacter {
                        file_pointer: self.file_pointer,
                        character: c,
                    });

                    self.report_error(&e);

                    // Error recovery: just ignore and parse things like 4__0 as 4.
                }
                last_char_underscore = true;
                self.advance();
            } else if c == '.' && base == 10 && !is_float {
                self.advance();
                if self.peek().map_or(false, |c| c.is_ascii_digit()) {
                    is_float = true;
                    has_digits = false;
                } else {
                    // Not a valid digit
                    let e = CompileError::Syntax(SyntaxError::InvalidNumberLiteral {
                        file_pointer: self.file_pointer,
                        literal: self
                            .unchecked_index_into_buffer(start, self.file_pointer.file_position)
                            .to_string(),
                    });
                    self.report_error(&e);

                    // Error recovery: treat as integer, ignoring the dot
                    self.advance();
                    break;
                }
            } else {
                break;
            }
        }

        // This prevents things like 0xfg, where the first character is valid but the rest isn't.
        // Normally, when peeked, the next character should either not exist, be EOF, newline,
        // or space. If the next character is a number, then there is something wrong.
        if let Some(c) = self.peek() {
            if c.is_alphanumeric() {
                dbg!("error occurring");
                let e = CompileError::Syntax(SyntaxError::InvalidNumberLiteral {
                    file_pointer: self.file_pointer,
                    literal: self
                        .unchecked_index_into_buffer(start, self.file_pointer.file_position + 1)
                        .to_string(),
                });

                self.report_error(&e);

                // Error recovery: Ignore the invalid suffix
                return self.recover_potential_number(line_start, is_float, c);
            }
        }

        if last_char_underscore || !has_digits {
            let e = CompileError::Syntax(SyntaxError::InvalidNumberLiteral {
                file_pointer: self.file_pointer,
                literal: self
                    .unchecked_index_into_buffer(start, self.file_pointer.file_position)
                    .to_string(),
            });

            self.report_error(&e);

            // Error recovery: Already handled in the previous step.
            // There are no digits, so return 0.
            return Token::Number(
                FilePointer {
                    file_position: start,
                    line_position: line_start,
                    ..self.file_pointer
                },
                "0".into(),
                false,
            );
        }

        let text = self
            .unchecked_index_into_buffer(start, self.file_pointer.file_position)
            .to_string()
            .replace('_', "");
        Token::Number(
            FilePointer {
                file_position: start,
                line_position: line_start,
                ..self.file_pointer
            },
            text,
            is_float,
        )
    }

    /// Try and recover a number or an identifier if an error has occurred.
    fn recover_potential_number(&mut self, line_start: usize, is_float: bool, c: char) -> Token {
        let start = self.file_pointer;
        return if c.is_ascii_digit() {
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
        };
    }

    /// Lex a string. Return a CompileError if any error has occurred.
    fn string(&mut self) -> Token {
        let start = self.file_pointer;
        let mut value = String::new();
        let mut has_advanced_once = false;
        while let Some(c) = self.advance() {
            has_advanced_once = true;
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
                        let e = CompileError::Syntax(SyntaxError::UnterminatedStringLiteral {
                            file_pointer: start,
                        });

                        self.report_error(&e);
                        // Just ignore the char
                    }
                }
            } else {
                if c == '\n' {
                    self.file_pointer.line += 1;
                    self.file_pointer.line_position = 1;

                    let e = CompileError::Syntax(SyntaxError::UnterminatedStringLiteral {
                        file_pointer: start,
                    });

                    self.report_error(&e);
                    // Ignore the line end, and terminate the string here
                    value.push('"');
                    break;
                }
                value.push(c);
            }
        }

        if (self.peek().is_none() && !self.buffer.ends_with('"')) || !has_advanced_once {
            let e = CompileError::Syntax(SyntaxError::UnterminatedStringLiteral {
                file_pointer: start,
            });

            self.report_error(&e);
            // Error already recovered, so it'll always end with "
        }

        Token::String(
            FilePointer {
                file_position: start.file_position,
                ..self.file_pointer
            },
            value,
        )
    }

    /// Advances the lexer while the current char
    /// is alphanumeric.
    fn advance_while_alphanumeric(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Revert to file pointer and attempt to lex an identifier.
    /// Useful for error recovery.
    fn revert_lex_identifier(&mut self, file_pointer: FilePointer, else_char: char) -> Token {
        match self.revert_to_position(file_pointer) {
            Err(ref e) => {
                self.report_error(e);
                Token::Eof(file_pointer)
            }
            Ok(_) => self.identifier_or_keyword(else_char),
        }
    }

    /// Indexes the buffer as [start, end).
    /// This method is preferred over `self.buffer` as it also takes starting point
    /// into account.
    fn unchecked_index_into_buffer(&self, start: usize, end: usize) -> &str {
        let buffer_start = start - self.buffer_start;
        let buffer_end = end - self.buffer_start;

        &self.buffer[buffer_start..buffer_end]
    }

    /// Advances the lexer by one position, returning the consumed char.
    fn advance(&mut self) -> Option<char> {
        if self.buffer_eof && self.buffer_position >= self.buffer.len() {
            return None;
        }

        if let Err(e) = self.refill_buffer() {
            self.report_error(&e);
            return None;
        }

        let ch = self.buffer[self.buffer_position - self.buffer_start..]
            .chars()
            .next()
            .unwrap();
        self.buffer_position += ch.len_utf8();
        self.file_pointer.file_position += ch.len_utf8();
        self.file_pointer.line_position += ch.len_utf8();
        Some(ch)
    }

    /// Looks ahead to the next token without advancing the token.
    fn peek(&mut self) -> Option<char> {
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
    fn match_next(&mut self, other: char) -> bool {
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
    fn match_next_next(&mut self, other: char) -> bool {
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
    fn match_base_prefix(&mut self) -> bool {
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
    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek() {
                Some(' ') | Some('\r') | Some('\t') => {
                    self.advance();
                }
                Some('\n') => {
                    self.advance();
                    self.file_pointer.line += 1;
                    self.file_pointer.line_position = 1;
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
                                self.file_pointer.line += 1;
                                self.file_pointer.line_position = 1;
                            }
                        }
                        if self.peek().is_none() && !was_broken {
                            let e = CompileError::Syntax(SyntaxError::UnterminatedComment {
                                file_pointer: self.file_pointer,
                            });
                            self.report_error(&e);
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

    /// Shorthand to report error, which delegates to
    /// the reporter field.
    pub fn report_error(&mut self, error: &CompileError) {
        self.context.reporter().report(error);
    }

    /// Returns true if the lexer is at end, false otherwise.
    pub fn is_at_end(&self) -> bool {
        self.buffer_eof && self.buffer_position >= self.buffer.len()
    }

    /// Refills the Lexer's buffer.
    ///
    /// Returns Ok(()) if no errors have occurred, and CompileError
    /// with an IO Error otherwise.
    fn refill_buffer(&mut self) -> Result<(), CompileError> {
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
