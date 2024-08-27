use crate::context::Position;
use crate::errors::{CompileError, SyntaxError};
use crate::parser::Parser;
use crate::token::{Token, TokenType};
use crate::{generate_consume_impl, generate_match_impl};

impl Parser {
    /// Shorthand to report errors.
    #[inline]
    #[allow(unused)]
    pub(super) fn report(&mut self, error: &CompileError) {
        self.lexer.report_error(error);
    }

    /// Advances the parser, consuming and returning the next token.
    pub(super) fn advance(&mut self) -> Token {
        if let Some(token) = self.token_queue.pop_front() {
            token
        } else {
            // let next_token_opt = self.lexer.request_next_token();
            self.lexer.request_next_token()
        }
    }

    /// Peeks the next token without consuming it.
    pub(super) fn peek(&mut self) -> Result<&Token, CompileError> {
        if self.token_queue.is_empty() {
            let token = self.lexer.request_next_token();
            self.token_queue.push_back(token);
        }
        Ok(self.token_queue.front().unwrap())
    }

    /// Recovers to the given position.
    pub(super) fn recover_to_position(&mut self, position: Position) -> Result<(), CompileError> {
        self.token_queue.clear();
        self.lexer.revert_to_position(position)
    }

    /// Delegates to the is_at_end function of the lexer.
    pub(super) fn at_end(&mut self) -> bool {
        (self.token_queue.is_empty() && self.lexer.is_at_end()) || self.match_eof()
    }

    /// Clears the token queue.
    pub(super) fn clear_queue(&mut self) {
        self.token_queue.clear();
    }
}

// Manually implemented consume_identifier, as generate_consume_impl is restricted to one token
impl Parser {
    pub(super) fn consume_identifier(&mut self) -> Result<Token, CompileError> {
        let token = self.peek()?;
        match token.token_type {
            TokenType::Identifier(..) | TokenType::Self_ => Ok(self.advance()),
            _ => {
                let e = CompileError::Syntax(SyntaxError::ExpectedDifferent {
                    span: token.span,
                    expected: "an identifier".to_string(),
                });
                Err(e)
            }
        }
    }
}

generate_consume_impl! {
    consume_string => TokenType::String(_), "a string literal",
    consume_lbrace => TokenType::LeftBrace, '{',
    consume_rbrace => TokenType::RightBrace, '}',
    consume_lparen => TokenType::LeftParen, '(',
    consume_rparen => TokenType::RightParen, ')',
    consume_comma => TokenType::Comma, ',',
    consume_colon => TokenType::Colon, ':',
    consume_rangle => TokenType::RightAngle, '>',
    consume_semicolon => TokenType::Semicolon, ';',
    consume_equal => TokenType::Equal, '=',
    consume_enum => TokenType::Enum, "enum",
    consume_error => TokenType::Error, "error",
    consume_func => TokenType::Func, "func",
    consume_struct => TokenType::Struct, "struct",
    consume_extension => TokenType::Extension, "extension",
    consume_let => TokenType::Let, "let",
    consume_use => TokenType::Use , "use",
    consume_return => TokenType::Return, "return",
    consume_for => TokenType::For , "for",
    consume_while => TokenType::While, "while",
    consume_in => TokenType::In, "in",
    consume_do => TokenType::Do , "do",
    consume_fat_arrow => TokenType::FatArrow, "=>",
    consume_lbracket => TokenType::LeftBracket, '[',
    consume_rbracket => TokenType::RightBracket, ']',
}

generate_match_impl! {
    match_number => TokenType::Integer(_) | TokenType::Float(_),
    match_bool => TokenType::True | TokenType::False,
    match_string => TokenType::String(_),
    match_lbrace => TokenType::LeftBrace,
    match_rbrace => TokenType::RightBrace,
    match_lparen => TokenType::LeftParen,
    match_rparen => TokenType::RightParen,
    match_comma => TokenType::Comma,
    match_semicolon => TokenType::Semicolon,
    match_colon => TokenType::Colon,
    match_colon_colon => TokenType::ColonColon,
    match_langle => TokenType::LeftAngle,
    match_thin_arrow => TokenType::ThinArrow,
    match_mut => TokenType::Mut,
    match_eof => TokenType::Eof,
    match_else => TokenType::Else,
    match_equal => TokenType::Equal,
    match_elif => TokenType::Elif,
    match_if => TokenType::If,
    match_match => TokenType::Match,
    match_bang => TokenType::Bang,
    match_minus => TokenType::Minus,
    match_slash => TokenType::Slash,
    match_star => TokenType::Star,
    match_plus => TokenType::Plus,
    match_ge => TokenType::RightAngle,
    match_geq => TokenType::GreaterEqual,
    match_le => TokenType::LeftAngle,
    match_leq => TokenType::LessEqual,
    match_bang_equal => TokenType::BangEqual,
    match_equal_equal => TokenType::EqualEqual,
    match_ampersand_ampersand => TokenType::AmpersandAmpersand,
    match_pipe_pipe => TokenType::PipePipe,
    match_dot => TokenType::Dot,
    match_underscore => TokenType::Underscore,
    match_lbracket => TokenType::LeftBracket,
    match_rbracket => TokenType::RightBracket,
    match_self => TokenType::Self_,
}
