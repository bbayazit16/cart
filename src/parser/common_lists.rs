use crate::ast;
use crate::errors::CompileError;
use crate::parser::Parser;

impl Parser {
    // enumTypeList   → type ( "," type )*
    pub(super) fn parse_typelist(&mut self) -> Result<Vec<ast::Type>, CompileError> {
        let mut types = vec![self.parse_type()?];

        while self.match_comma() {
            self.advance();
            types.push(self.parse_type()?);
        }

        Ok(types)
    }

    // structList     → structField ( "," structField )*
    pub(super) fn parse_struct_list(&mut self) -> Result<Vec<ast::StructField>, CompileError> {
        let mut struct_list = vec![self.parse_struct_field()?];

        // Do not change this to !self.match_rbrace.
        // In addition to parsing structs with braces, this function can
        // be used to parse errors, which contain structFields wrapped
        // around parens such as SomeError(x: type).
        while self.match_comma() {
            self.advance();
            struct_list.push(self.parse_struct_field()?);
        }

        Ok(struct_list)
    }

    // arguments      → expression ( "," expression )* ;
    pub(super) fn parse_arguments(&mut self) -> Result<Vec<ast::Expr>, CompileError> {
        let mut arguments = vec![self.parse_expr()];
        while self.match_comma() {
            self.advance();
            arguments.push(self.parse_expr());
        }
        Ok(arguments)
    }

    // parameters     → parameter ( "," parameter )* ;
    pub(super) fn parse_params(&mut self) -> Result<Vec<ast::Parameter>, CompileError> {
        let mut params = vec![self.parse_param()?];

        while self.match_comma() {
            self.advance();
            params.push(self.parse_param()?);
        }

        Ok(params)
    }
}
