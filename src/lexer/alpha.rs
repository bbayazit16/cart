use crate::context::FilePointer;
use crate::errors::{CompileError, SyntaxError};
use crate::lexer::Lexer;
use crate::token::Token;

impl Lexer {
    /// Return the next token from the input.
    pub(super) fn next_alpha(&mut self) -> Token {
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
    /// Lex an identifier or a keyword.
    pub(super) fn identifier_or_keyword(&mut self, initial_char: char) -> Token {
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
            "self" => Token::Self_(starting_file_position),
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
    pub(super) fn number(&mut self, initial_char: char) -> Token {
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

    /// Lex a string. Return a CompileError if any error has occurred.
    pub(super) fn string(&mut self) -> Token {
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
}
