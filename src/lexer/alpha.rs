use crate::context::{Position, Span};
use crate::errors::{CompileError, SyntaxError};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

macro_rules! basic_token {
    ($token:ident, $position:expr) => {
        Token {
            span: Span::single($position),
            token_type: TokenType::$token,
        }
    };
    ($self_:expr, $token:ident, $start:expr) => {
        Token {
            span: Span::new($start, $self_.position),
            token_type: TokenType::$token,
        }
    };
}

impl Lexer {
    /// Return the next token from the input.
    pub(super) fn next_alpha(&mut self) -> Token {
        self.skip_whitespace_and_comments();
        let start = self.position;
        match self.advance() {
            Some(current_char) => {
                match current_char {
                    '(' => basic_token!(LeftParen, start),
                    ')' => basic_token!(RightParen, start),
                    '{' => basic_token!(LeftBrace, start),
                    '}' => basic_token!(RightBrace, start),
                    '[' => basic_token!(LeftBracket, start),
                    ']' => basic_token!(RightBracket, start),
                    ',' => basic_token!(Comma, start),
                    '.' => basic_token!(Dot, start),
                    ';' => basic_token!(Semicolon, start),
                    '^' => basic_token!(Caret, start),
                    '~' => basic_token!(Tilde, start),
                    '?' => basic_token!(Question, start),
                    '@' => basic_token!(At, start),
                    '-' => {
                        if self.match_next('>') {
                            basic_token!(self, ThinArrow, start)
                        } else if self.match_next('=') {
                            basic_token!(self, MinusEqual, start)
                        } else {
                            basic_token!(Minus, start)
                        }
                    }
                    '+' => {
                        if self.match_next('=') {
                            basic_token!(self, PlusEqual, start)
                        } else {
                            basic_token!(Plus, start)
                        }
                    }
                    ':' => {
                        if self.match_next(':') {
                            basic_token!(self, ColonColon, start)
                        } else {
                            basic_token!(Colon, start)
                        }
                    }
                    '/' => {
                        if self.match_next('=') {
                            basic_token!(self, SlashEqual, start)
                        } else {
                            basic_token!(Slash, start)
                        }
                    }
                    '*' => {
                        if self.match_next('=') {
                            basic_token!(self, StarEqual, start)
                        } else {
                            basic_token!(Star, start)
                        }
                    }
                    '%' => {
                        if self.match_next('=') {
                            basic_token!(self, PercentEqual, start)
                        } else {
                            basic_token!(Percent, start)
                        }
                    }
                    '&' => {
                        if self.match_next('&') {
                            basic_token!(self, AmpersandAmpersand, start)
                        } else {
                            basic_token!(Ampersand, start)
                        }
                    }
                    '|' => {
                        if self.match_next('|') {
                            basic_token!(self, PipePipe, start)
                        } else {
                            basic_token!(Pipe, start)
                        }
                    }
                    '!' => {
                        if self.match_next('=') {
                            basic_token!(self, BangEqual, start)
                        } else {
                            basic_token!(Bang, start)
                        }
                    }
                    '=' => {
                        if self.match_next('=') {
                            basic_token!(self, EqualEqual, start)
                        } else if self.match_next('>') {
                            basic_token!(self, FatArrow, start)
                        } else {
                            basic_token!(Equal, start)
                        }
                    }
                    '>' => {
                        if self.match_next('=') {
                            basic_token!(self, GreaterEqual, start)
                        } else {
                            basic_token!(RightAngle, start)
                        }
                    }
                    '<' => {
                        if self.match_next('=') {
                            basic_token!(self, LessEqual, start)
                        } else {
                            basic_token!(LeftAngle, start)
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
                            self.report_error(&CompileError::Syntax(
                                SyntaxError::UnexpectedCharacter {
                                    span: Span::single(start),
                                    character: current_char,
                                },
                            ));

                            // Error recovery: attempt to parse as an identifier
                            self.identifier_or_keyword(current_char)
                        }
                    }
                }
            }
            None => basic_token!(Eof, start),
        }
    }
    /// Lex an identifier or a keyword.
    pub(super) fn identifier_or_keyword(&mut self, initial_char: char) -> Token {
        let start = Position {
            line: self.position.line,
            column: self.position.column - initial_char.len_utf8(),
            offset: self.position.offset - initial_char.len_utf8(),
        };

        self.advance_while_alphanumeric();

        let text = self.unchecked_index_into_buffer(start.offset, self.position.offset);
        match text {
            "self" => basic_token!(self, Self_, start),
            "enum" => basic_token!(self, Enum, start),
            "error" => basic_token!(self, Error, start),
            "and" => basic_token!(self, And, start),
            "async" => basic_token!(self, Async, start),
            "await" => basic_token!(self, Await, start),
            "struct" => basic_token!(self, Struct, start),
            "match" => basic_token!(self, Match, start),
            "else" => basic_token!(self, Else, start),
            "elif" => basic_token!(self, Elif, start),
            "false" => basic_token!(self, False, start),
            "true" => basic_token!(self, True, start),
            "func" => basic_token!(self, Func, start),
            "for" => basic_token!(self, For, start),
            "if" => basic_token!(self, If, start),
            "let" => basic_token!(self, Let, start),
            "mut" => basic_token!(self, Mut, start),
            "or" => basic_token!(self, Or, start),
            "return" => basic_token!(self, Return, start),
            "while" => basic_token!(self, While, start),
            "use" => basic_token!(self, Use, start),
            "extension" => basic_token!(self, Extension, start),
            "implements" => basic_token!(self, Implements, start),
            "do" => basic_token!(self, Do, start),
            "in" => basic_token!(self, In, start),
            "_" => basic_token!(self, Underscore, start),
            _ => Token {
                token_type: TokenType::Identifier(text.to_string()),
                span: Span::new(start, self.position),
            },
        }
    }

    /// Lex a number. Return CompileError if any error has occurred.
    pub(super) fn number(&mut self, initial_char: char) -> Token {
        let start = Position {
            line: self.position.line,
            column: self.position.column - initial_char.len_utf8(),
            offset: self.position.offset - initial_char.len_utf8(),
        };

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
                    // When reporting errors, include the character that caused the error
                    self.report_error(&CompileError::Syntax(SyntaxError::InvalidNumberLiteral {
                        span: Span::new(
                            start,
                            Position {
                                line: self.position.line,
                                column: self.position.column + 1,
                                offset: self.position.offset + 1,
                            },
                        ),
                        literal: format!("0{}", c),
                    }));

                    // Error recovery:
                    // Consume the unknown base
                    self.advance();
                    // Try parsing the number again
                    return self.recover_potential_number(is_float, c);
                }
                _ => {
                    // 0
                    return Token {
                        token_type: TokenType::Integer("0".into()),
                        span: Span::new(start, self.position),
                    };
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
                    self.report_error(&CompileError::Syntax(SyntaxError::UnexpectedCharacter {
                        span: Span::new(
                            start,
                            Position {
                                line: self.position.line,
                                column: self.position.column + 1,
                                offset: self.position.offset + 1,
                            },
                        ),
                        character: c,
                    }))

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
                    self.report_error(&CompileError::Syntax(SyntaxError::InvalidNumberLiteral {
                        span: Span::new(start, self.position),
                        literal: self
                            .unchecked_index_into_buffer(start.offset, self.position.offset)
                            .to_string(),
                    }));

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
                self.report_error(&CompileError::Syntax(SyntaxError::InvalidNumberLiteral {
                    span: Span::new(
                        start,
                        Position {
                            line: self.position.line,
                            column: self.position.column + 1,
                            offset: self.position.offset + 1,
                        },
                    ),
                    literal: self
                        .unchecked_index_into_buffer(start.offset, self.position.offset + 1)
                        .to_string(),
                }));

                // Error recovery: Ignore the invalid suffix
                return self.recover_potential_number(is_float, c);
            }
        }

        if last_char_underscore || !has_digits {
            self.report_error(&CompileError::Syntax(SyntaxError::InvalidNumberLiteral {
                span: Span::new(start, self.position),
                literal: self
                    .unchecked_index_into_buffer(start.offset, self.position.offset)
                    .to_string(),
            }));

            // Error recovery: Already handled in the previous step.
            // There are no digits, so return 0.
            return Token {
                token_type: TokenType::Integer("0".into()),
                span: Span::new(start, self.position),
            };
        }

        let text = self
            .unchecked_index_into_buffer(start.offset, self.position.offset)
            .to_string()
            .replace('_', "");

        Token {
            token_type: if is_float {
                TokenType::Float(text)
            } else {
                TokenType::Integer(text)
            },
            span: Span::new(start, self.position),
        }
    }

    /// Lex a string. Return a CompileError if any error has occurred.
    pub(super) fn string(&mut self) -> Token {
        // 1 subtracted as the current character " is already consumed
        let start = Position {
            line: self.position.line,
            column: self.position.column - 1,
            offset: self.position.offset - 1,
        };

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
                        self.report_error(&CompileError::Syntax(
                            SyntaxError::UnterminatedStringLiteral {
                                span: Span::new(start, self.position),
                            },
                        ));

                        // Error recovery: just ignore the char
                    }
                }
            } else if c == '\n' {
                self.position.line += 1;
                self.position.column = 1;

                self.report_error(&CompileError::Syntax(
                    SyntaxError::UnterminatedStringLiteral {
                        span: Span::new(start, self.position),
                    },
                ));

                // Error recovery: ignore the line end, and terminate the string here
                break;
            } else {
                value.push(c);
            }
        }

        if (self.peek().is_none() && !self.buffer.ends_with('"')) || !has_advanced_once {
            self.report_error(&CompileError::Syntax(
                SyntaxError::UnterminatedStringLiteral {
                    span: Span::new(start, self.position),
                },
            ));
            // Error already recovered, so it'll always end with "
        }

        Token {
            token_type: TokenType::String(value),
            span: Span::new(start, self.position),
        }
    }
}
