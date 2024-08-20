use crate::context::FilePointer;
use crate::errors::{CompileError, SyntaxError};
use crate::parser::Parser;
use crate::token::Token;
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

    /// Recovers to the given token.
    #[allow(unused)]
    pub(super) fn recover_to_token(&mut self, token: &Token) -> Result<(), CompileError> {
        self.token_queue.clear();
        self.lexer.revert_to_position(token.get_file_pointer())
    }

    /// Recovers to the given position.
    pub(super) fn recover_to_position(
        &mut self,
        position: FilePointer,
    ) -> Result<(), CompileError> {
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
        let token = self.advance();
        match token {
            Token::Identifier(..) | Token::Self_(..) => Ok(token),
            _ => {
                let e = CompileError::Syntax(SyntaxError::ExpectedDifferentCharacter {
                    file_pointer: token.get_file_pointer(),
                    expected: "an identifier".to_string(),
                });
                Err(e)
            }
        }
    }
}

generate_consume_impl! {
    consume_string => Token::String(..), "a string literal",
    consume_lbrace => Token::LeftBrace(..), '{',
    consume_rbrace => Token::RightBrace(..), '}',
    consume_lparen => Token::LeftParen(..), '(',
    consume_rparen => Token::RightParen(..), ')',
    consume_comma => Token::Comma(..), ',',
    consume_colon => Token::Colon(..), ':',
    consume_rangle => Token::RightAngle(..), '>',
    consume_semicolon => Token::Semicolon(..), ';',
    consume_equal => Token::Equal(..), '=',
    consume_enum => Token::Enum(..), "enum",
    consume_error => Token::Error(..), "error",
    consume_func => Token::Func(..), "func",
    consume_struct => Token::Struct(..), "struct",
    consume_extension => Token::Extension(..), "extension",
    consume_let => Token::Let(..), "let",
    consume_use => Token::Use(..) , "use",
    consume_return => Token::Return(..) , "return",
    consume_for => Token::For(..) , "for",
    consume_while => Token::While(..) , "while",
    consume_in => Token::In(..), "in",
    consume_do => Token::Do(..) , "do",
    consume_fat_arrow => Token::FatArrow(..), "=>",
    consume_lbracket => Token::LeftBracket(..), '[',
    consume_rbracket => Token::RightBracket(..), ']',
}

generate_match_impl! {
    // match_identifier => Token::Identifier(..),
    match_number => Token::Number(..),
    match_bool => Token::True(..) | Token::False(..),
    match_string => Token::String(..),
    match_lbrace => Token::LeftBrace(..),
    match_rbrace => Token::RightBrace(..),
    match_lparen => Token::LeftParen(..),
    match_rparen => Token::RightParen(..),
    match_comma => Token::Comma(..),
    match_semicolon => Token::Semicolon(..),
    match_colon => Token::Colon(..),
    match_colon_colon => Token::ColonColon(..),
    match_langle => Token::LeftAngle(..),
    match_thin_arrow => Token::ThinArrow(..),
    match_mut => Token::Mut(..),
    match_eof => Token::Eof(..),
    match_else => Token::Else(..),
    match_equal => Token::Equal(..),
    match_elif => Token::Elif(..),
    match_if => Token::If(..),
    match_match => Token::Match(..),
    match_bang => Token::Bang(..),
    match_minus => Token::Minus(..),
    match_slash => Token::Slash(..),
    match_star => Token::Star(..),
    match_plus => Token::Plus(..),
    match_ge => Token::RightAngle(..),
    match_geq => Token::GreaterEqual(..),
    match_le => Token::LeftAngle(..),
    match_leq => Token::LessEqual(..),
    match_bang_equal => Token::BangEqual(..),
    match_equal_equal => Token::EqualEqual(..),
    match_ampersand_ampersand => Token::AmpersandAmpersand(..),
    match_pipe_pipe => Token::PipePipe(..),
    match_dot => Token::Dot(..),
    match_underscore => Token::Underscore(..),
    match_lbracket => Token::LeftBracket(..),
    match_rbracket => Token::RightBracket(..),
    match_self => Token::Self_(..),
}
