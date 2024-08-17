//! This module implements a recursive descent parser. The parser takes a sequence of tokens,
//! which are produced on the fly by the [`Lexer`], and outputs `Program`, the Abstract
//! Syntax Tree (AST).
//!
//! The `Parser` struct is the main component of the parser. It uses `Lexer` to obtain tokens
//! and reports errors via the lexer's `Reporter` instance. To implement peeking, the tokens are
//! put into a `VecDeque`.
//!
//! The parser attempts to parse three main constructs: Declarations, Statements, and Expressions.
//!
//! To see proper documentation of the grammar, see [`grammar`].
//!
//! This module provides macros. Macros automatically implement token matching for the `Parser`.
//!
//! These macros are used in parsing to implement certain functions that match certain Tokens
//! outputted by the Lexer.
//!
//! The macros also provide default error recovery, and report errors to the [`Reporter`].

use crate::ast;
use crate::context::FileContext;
use crate::errors::CompileError;
use crate::lexer::Lexer;
use crate::token::Token;
use std::collections::VecDeque;

mod blocks;
mod common;
mod common_lists;
mod declarations;
mod expressions;
mod macro_utils;
mod match_arms;
mod patterns;
mod statements;
mod structs_enums;
mod types;

/// The `Parser` struct is responsible for parsing a sequence of tokens into
/// `Program`, an Abstract Syntax Tree (AST).
///
/// Tokens are produced on the fly by the `Lexer`, and put into `VecDeque` to implement peeking.
/// If a syntax error occurs, the parser reports it and attempts to continue parsing.
///
/// The parser's methods correspond to the grammar rules of the language and are responsible for
/// creating the appropriate AST nodes. If a syntax error is encountered, the parser will report it
/// and attempt to continue parsing.
///
/// # Fields
///
/// - `lexer`: An instance of `Lexer` that provides the tokens to be parsed.
/// - `errors`: A vector of compile-time errors that occurred when parsing.
/// - `token_queue`: A `VecDeque` of `Token`s that serves as a buffer for the tokens being processed.
///
pub(crate) struct Parser {
    lexer: Lexer,
    errors: Vec<CompileError>,
    token_queue: VecDeque<Token>,
}

impl Parser {
    /// Initialize a new parser using `FileContext`.
    pub(crate) fn new(context: FileContext) -> Self {
        Parser {
            lexer: Lexer::new(context),
            errors: Vec::new(),
            token_queue: VecDeque::new(),
        }
    }

    /// Parses the entire program, returning CompileError or
    /// the output Program, a vector of declaration ASTs.
    pub(crate) fn parse(&mut self) -> ast::Program {
        let mut declarations = Vec::new();
        while !self.at_end() {
            let start_position = self.peek().unwrap().get_file_pointer();
            match self.parse_declaration() {
                Ok(declaration) => declarations.push(declaration),
                Err(e) => {
                    self.errors.push(e);
                    self.recover_to_position(start_position).unwrap();
                    self.synchronize();
                }
            }
            self.clear_queue();
        }
        for error in self.errors.iter() {
            self.lexer.report_error(error);
        }
        ast::Program { declarations }
    }

    /// Synchronizes the parser into the next correct position,
    /// which would be the point where a major construct is encountered.
    fn synchronize(&mut self) {
        // let previous = self.advance();

        while !self.at_end() {
            // if matches!(previous, Token::Semicolon(..)) { return };
            if self.match_semicolon() {
                self.advance();
                return;
            }
            match self.peek() {
                Ok(Token::Struct(..))
                | Ok(Token::Enum(..))
                | Ok(Token::Func(..))
                | Ok(Token::Let(..))
                | Ok(Token::For(..))
                | Ok(Token::While(..))
                | Ok(Token::If(..))
                | Ok(Token::Return(..)) => return,
                _ => {
                    self.advance();
                }
            }
        }
    }
}
