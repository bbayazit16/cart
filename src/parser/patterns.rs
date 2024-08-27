use crate::ast;
use crate::errors::CompileError;
use crate::parser::Parser;
use crate::token::Token;

impl Parser {
    // pattern        → IDENTIFIER ( ( "(" identList ")" )? | ( "{" identList "}" ) )
    //                | "_" ;
    pub(super) fn parse_pattern(&mut self) -> Result<ast::Pattern, CompileError> {
        if self.match_underscore() {
            let underscore_span = self.advance().span;
            Ok(ast::Pattern::Wildcard(underscore_span))
        } else {
            let ident = self.consume_identifier()?;
            if self.match_lparen() {
                self.advance();
                let ident_list = self.parse_ident_list()?;
                let rparen_span = self.consume_rparen()?.span;
                Ok(ast::Pattern::EnumOrStructVariant(
                    ident.span.merge(&rparen_span),
                    ast::PatternType::Enum,
                    ident,
                    ident_list,
                ))
            } else if self.match_lbrace() {
                self.advance();
                let ident_list = self.parse_ident_list()?;
                let rbrace_span = self.consume_rbrace()?;
                Ok(ast::Pattern::EnumOrStructVariant(
                    ident.span.merge(&rbrace_span.span),
                    ast::PatternType::Struct,
                    ident,
                    ident_list,
                ))
            } else {
                Ok(ast::Pattern::Identifier(ident))
            }
        }
    }

    // identList          → IDENTIFIER ( "," IDENTIFIER )* ;
    fn parse_ident_list(&mut self) -> Result<Vec<Token>, CompileError> {
        let mut ident_list = vec![self.consume_identifier()?];
        while self.match_comma() {
            self.advance();
            ident_list.push(self.consume_identifier()?);
        }
        Ok(ident_list)
    }
}
