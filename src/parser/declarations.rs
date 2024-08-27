use crate::ast;
use crate::errors::CompileError;
use crate::parser::Parser;
use crate::token::TokenType;

impl Parser {
    /// Parse and return a declaration.
    pub(super) fn parse_declaration(&mut self) -> ast::Declaration {
        match self.parse_declaration_() {
            Ok(declaration) => declaration,
            Err(e) => {
                self.errors.push(e);
                self.synchronize();
                ast::Declaration::NotRecovered
            }
        }
    }

    fn parse_declaration_(&mut self) -> Result<ast::Declaration, CompileError> {
        match self.peek()?.token_type {
            TokenType::Enum => Ok(ast::Declaration::EnumDecl(self.parse_enum()?)),
            TokenType::Error => Ok(ast::Declaration::ErrorDecl(self.parse_error()?)),
            TokenType::Func => Ok(ast::Declaration::FunctionDecl(self.parse_function()?)),
            TokenType::Struct => Ok(ast::Declaration::StructDecl(self.parse_struct()?)),
            TokenType::Extension => Ok(ast::Declaration::ExtensionDecl(self.parse_extension()?)),
            _ => Ok(ast::Declaration::StatementDecl(self.parse_stmt())),
        }
    }

    // enumDecl     → "enum" IDENTIFIER "{" enumVariantList? "}" ;
    fn parse_enum(&mut self) -> Result<ast::EnumDecl, CompileError> {
        let starting_span = self.consume_enum()?.span;
        let identifier = self.consume_identifier()?;
        self.consume_lbrace()?;

        let variants = self.parse_enum_variant_list()?;

        let ending_span = self.consume_rbrace()?.span;

        Ok(ast::EnumDecl {
            name: identifier,
            variants,
            span: starting_span.merge(&ending_span),
        })
    }

    // errorDecl           → "error" IDENTIFIER "{" errorVariantList* "}" ;
    fn parse_error(&mut self) -> Result<ast::ErrorDecl, CompileError> {
        let starting_span = self.consume_error()?.span;
        let name = self.consume_identifier()?;
        self.consume_lbrace()?;

        let variants = self.parse_error_variant_list()?;

        let ending_span = self.consume_rbrace()?.span;

        Ok(ast::ErrorDecl {
            name,
            variants,
            span: starting_span.merge(&ending_span),
        })
    }

    // function       → "func" IDENTIFIER genericArgs? "(" parameters? ")"
    //                  ( "->" type )?
    //                  block ;
    fn parse_function(&mut self) -> Result<ast::FunctionDecl, CompileError> {
        let starting_span = self.consume_func()?.span;
        let name = self.consume_identifier()?;

        let generic_params = self.parse_optional_generic_params()?;

        self.consume_lparen()?;

        let params = if self.match_rparen() {
            Vec::new()
        } else {
            self.parse_params()?
        };

        self.consume_rparen()?;

        let return_type = if self.match_thin_arrow() {
            self.advance();
            self.parse_type()?
        } else {
            ast::Type::Empty(name.span)
        };

        let body = self.parse_block()?;
        let ending_span = body.span;

        Ok(ast::FunctionDecl {
            name,
            params,
            return_type,
            body,
            generic_params,
            is_self: false,
            span: starting_span.merge(&ending_span),
        })
    }

    // structDecl          → "struct" IDENTIFIER genericArgs? "{" structList? "}" ;
    fn parse_struct(&mut self) -> Result<ast::StructDecl, CompileError> {
        let starting_span = self.consume_struct()?.span;
        let name = self.consume_identifier()?;

        let generic_params = self.parse_optional_generic_params()?;

        self.consume_lbrace()?;
        let fields = if self.match_rbrace() {
            self.advance();
            Vec::new()
        } else {
            self.parse_struct_list()?
        };

        let ending_span = self.consume_rbrace()?.span;

        Ok(ast::StructDecl {
            name,
            fields,
            generic_params,
            span: starting_span.merge(&ending_span),
        })
    }

    // extensionDecl       → "extension" IDENTIFIER genericArgs? "{" funcDecl* "}" ;
    fn parse_extension(&mut self) -> Result<ast::ExtensionDecl, CompileError> {
        let starting_span = self.consume_extension()?.span;

        let name = self.consume_identifier()?;

        let generic_params = self.parse_optional_generic_params()?;

        let header_end_span = self.consume_lbrace()?.span;

        let mut functions = Vec::new();
        while !self.match_rbrace() && !self.at_end() {
            functions.push(self.parse_self_permitting_func()?);
        }

        let ending_span = self.consume_rbrace()?.span;

        Ok(ast::ExtensionDecl {
            name,
            generic_params,
            functions,
            span: starting_span.merge(&ending_span),
            header_span: starting_span.merge(&header_end_span),
        })
    }

    // selfPermFuncDecl    → "func" selfPermittingFunc ;
    // selfPermittingFunc → IDENTIFIER genericArgs? "(" "self"? ( "," parameters )? ")"
    //                      ( "->" type )?
    //                      block ;
    fn parse_self_permitting_func(&mut self) -> Result<ast::FunctionDecl, CompileError> {
        let starting_span = self.consume_func()?.span;
        let name = self.consume_identifier()?;

        let generic_params = self.parse_optional_generic_params()?;

        self.consume_lparen()?;

        // let params = if self.match_rparen() {
        //     Vec::new()
        // } else {
        //     self.parse_params()?
        // };
        let (params, is_self) = if self.match_rparen() {
            (Vec::new(), false)
        } else if self.match_self() {
            self.advance(); // consume "self"
            if self.match_comma() {
                self.advance(); // consume ",'
                let params = self.parse_params()?;
                (params, true)
            } else {
                (Vec::new(), true)
            }
        } else {
            let params = self.parse_params()?;
            (params, false)
        };

        self.consume_rparen()?;

        let return_type = if self.match_thin_arrow() {
            self.advance();
            self.parse_type()?
        } else {
            ast::Type::Empty(name.span)
        };

        let body = self.parse_block()?;
        let ending_span = body.span;

        Ok(ast::FunctionDecl {
            name,
            params,
            return_type,
            body,
            generic_params,
            is_self,
            span: starting_span.merge(&ending_span),
        })
    }
}
