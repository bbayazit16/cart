use crate::ast;
use crate::errors::CompileError;
use crate::parser::Parser;
use crate::token::Token;

impl Parser {
    // structLiteral  → IDENTIFIER "{" structParamValues? "}" ;
    pub(super) fn parse_struct_literal(
        &mut self,
        existing_name: Option<Token>,
    ) -> Result<ast::StructLiteral, CompileError> {
        let name = match existing_name {
            Some(exn) => exn,
            None => self.consume_identifier()?,
        };

        self.consume_lbrace()?;
        let fields = if self.match_rbrace() {
            Vec::new()
        } else {
            self.parse_struct_param_values()?
        };
        self.consume_rbrace()?;

        Ok(ast::StructLiteral { name, fields })
    }

    // structParamValues  → structParamValue ( "," structParamValue )* ;
    fn parse_struct_param_values(&mut self) -> Result<Vec<(Token, ast::Expr)>, CompileError> {
        let mut params = vec![self.parse_struct_param_value()?];
        while self.match_comma() {
            self.advance();
            params.push(self.parse_struct_param_value()?);
        }
        Ok(params)
    }

    // structParamValue   → IDENTIFIER ":" expression ;
    fn parse_struct_param_value(&mut self) -> Result<(Token, ast::Expr), CompileError> {
        let ident = self.consume_identifier()?;
        self.consume_colon()?;
        let expr = self.parse_expr()?;
        Ok((ident, expr))
    }

    // structField    → IDENTIFIER ":" type ;
    pub(super) fn parse_struct_field(&mut self) -> Result<ast::StructField, CompileError> {
        let identifier = self.consume_identifier()?;
        self.consume_colon()?;
        let field_type = self.parse_type()?;

        Ok(ast::StructField {
            name: identifier,
            field_type,
        })
    }

    // enumValue      → IDENTIFIER ( "::" IDENTIFIER )? ( "(" arguments? ")" )? ;
    pub(super) fn parse_enum_value(
        &mut self,
        existing_name: Option<Token>,
    ) -> Result<ast::EnumValue, CompileError> {
        let name = match existing_name {
            Some(exn) => exn,
            None => self.consume_identifier()?,
        };

        let variant = if self.match_colon_colon() {
            self.advance();
            Some(self.consume_identifier()?)
        } else {
            None
        };

        // TODO: error resolution via context

        if !self.match_lparen() {
            return Ok(ast::EnumValue {
                name,
                variant,
                arguments: Vec::new(),
            });
        }

        // Consume '('
        self.advance();

        let arguments = if self.match_rparen() {
            // Directly closed, i.e, Enum::Ident()
            // Has no effect; the same as Enum::Ident
            Vec::new()
        } else {
            self.parse_arguments()?
        };

        self.advance();

        Ok(ast::EnumValue {
            name,
            variant,
            arguments,
        })
    }

    // enumVariant    → IDENTIFIER ( "(" enumTypeList ")" )?
    //                | IDENTIFIER ( "{" structList "}" ) ;
    fn parse_enum_variant(&mut self) -> Result<ast::EnumVariant, CompileError> {
        let identifier = self.consume_identifier()?;
        if self.match_lparen() {
            self.advance();
            // Then expect an enum typelist
            let typelist = self.parse_typelist()?;
            self.consume_rparen()?;

            Ok(ast::EnumVariant::Tuple(identifier, typelist))
        } else if self.match_lbrace() {
            self.advance();
            let struct_list = self.parse_struct_list()?;
            self.consume_rbrace()?;

            Ok(ast::EnumVariant::Struct(identifier, struct_list))
        } else {
            Ok(ast::EnumVariant::Simple(identifier))
        }
    }

    // enumVariantList     → enumVariant ( "," enumVariant )* ;
    pub(super) fn parse_enum_variant_list(
        &mut self,
    ) -> Result<Vec<ast::EnumVariant>, CompileError> {
        let mut variants = Vec::new();
        if self.match_rbrace() {
            self.advance();
            return Ok(variants);
        } else {
            variants.push(self.parse_enum_variant()?);
        }

        while !self.match_rbrace() && !self.at_end() {
            self.consume_comma()?;
            variants.push(self.parse_enum_variant()?);
        }

        Ok(variants)
    }

    // parameter      → IDENTIFIER ":" type ;
    pub(super) fn parse_param(&mut self) -> Result<ast::Parameter, CompileError> {
        let name = self.consume_identifier()?;
        self.consume_colon()?;
        let param_type = self.parse_type()?;

        Ok(ast::Parameter { name, param_type })
    }

    // Return a StructAccessExpr, if exists.
    pub(super) fn partial_parse_struct_access(
        &mut self,
        object: ast::Expr,
    ) -> Result<ast::StructAccessExpr, CompileError> {
        // TODO: Reporter - temp. variable
        // TODO: Handle expr properly
        let mut fields = vec![self.consume_identifier()?];
        while self.match_dot() {
            self.advance();
            fields.push(self.consume_identifier()?);
        }

        Ok(ast::StructAccessExpr { object, fields })
    }

    // errorVariant  → IDENTIFIER ( "(" structList ")" )? "=" STRING_LITERAL ;
    fn parse_error_variant(&mut self) -> Result<ast::ErrorVariant, CompileError> {
        let name = self.consume_identifier()?;
        let fields = if self.match_lparen() {
            self.advance();
            let result = self.parse_struct_list()?;
            self.consume_rparen()?;
            result
        } else {
            Vec::new()
        };

        self.consume_equal()?;
        let message = self.consume_string()?;

        Ok(ast::ErrorVariant {
            name,
            fields,
            message,
        })
    }

    // errorVariantList    → errorVariant ( "," errorVariant )* ;
    pub(super) fn parse_error_variant_list(
        &mut self,
    ) -> Result<Vec<ast::ErrorVariant>, CompileError> {
        let mut variants = Vec::new();
        if self.match_rbrace() {
            self.advance();
            return Ok(variants);
        } else {
            variants.push(self.parse_error_variant()?);
        }

        while !self.match_rbrace() && !self.at_end() {
            self.consume_comma()?;
            variants.push(self.parse_error_variant()?);
        }

        Ok(variants)
    }
}
