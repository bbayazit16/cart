use crate::ast;
use crate::errors::CompileError;
use crate::parser::Parser;

impl Parser {
    // type           → IDENTIFIER genericArgs?
    //                | "(" type? ")"
    pub(super) fn parse_type(&mut self) -> Result<ast::Type, CompileError> {
        if self.match_lparen() {
            let l_paren_span = self.advance().span;
            if self.match_rparen() {
                let r_paren_span = self.advance().span;
                // If (), return empty type
                Ok(ast::Type::Empty(l_paren_span.merge(&r_paren_span)))
            } else {
                // Return the inner type from the nested type
                let inner_type = self.parse_type()?;
                // Span isn't really important here. If an error occurs in the inner type,
                // it will be reported accordingly. Parens are just for grouping.
                Ok(inner_type)
            }
        } else {
            // Then, either IDENTIFIER genericArgs? OR type "|" type
            let identifier = self.consume_identifier()?;
            if self.match_langle() {
                // IDENTIFIER genericsArgs
                self.advance();
                let generic_list = self.parse_typelist()?;
                let right_angle_span = self.consume_rangle()?.span;
                Ok(ast::Type::Generic(
                    identifier.span.merge(&right_angle_span),
                    identifier,
                    generic_list,
                ))
            } else {
                Ok(ast::Type::Simple(identifier))
            }
        }
    }

    /// Checks for '<', and returns `Vec<Type>` of generic params.
    /// Consumes both '<' and '>'.
    /// If there are no generic types, returns an empty vector
    /// without consuming anything.
    pub(super) fn parse_optional_generic_params(&mut self) -> Result<Vec<ast::Type>, CompileError> {
        if self.match_langle() {
            self.advance();
            let generic_list = self.parse_typelist()?;
            self.consume_rangle()?;
            Ok(generic_list)
        } else {
            Ok(Vec::new())
        }
    }
}
