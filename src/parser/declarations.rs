use crate::ast;
use crate::errors::CompileError;
use crate::parser::Parser;
use crate::token::Token;

impl Parser {
    /// Parse and return a declaration.
    pub(super) fn parse_declaration(&mut self) -> Result<ast::Declaration, CompileError> {
        match self.peek()? {
            Token::Enum(..) => Ok(ast::Declaration::EnumDecl(Box::new(self.parse_enum()?))),
            Token::Error(..) => Ok(ast::Declaration::ErrorDecl(Box::new(self.parse_error()?))),
            Token::Func(..) => Ok(ast::Declaration::FunctionDecl(Box::new(
                self.parse_function()?,
            ))),
            Token::Struct(..) => Ok(ast::Declaration::StructDecl(Box::new(self.parse_struct()?))),
            Token::Extension(..) => Ok(ast::Declaration::ExtensionDecl(Box::new(
                self.parse_extension()?,
            ))),
            _ => Ok(ast::Declaration::StatementDecl(Box::new(
                self.parse_stmt()?,
            ))),
        }
    }

    // enumDecl     → "enum" IDENTIFIER "{" enumVariantList? "}" ;
    fn parse_enum(&mut self) -> Result<ast::EnumDecl, CompileError> {
        self.consume_enum()?;
        let identifier = self.consume_identifier()?;
        self.consume_lbrace()?;

        let variants = self.parse_enum_variant_list()?;

        self.consume_rbrace()?;

        Ok(ast::EnumDecl {
            name: identifier,
            variants,
        })
    }

    // errorDecl           → "error" IDENTIFIER "{" errorVariantList* "}" ;
    fn parse_error(&mut self) -> Result<ast::ErrorDecl, CompileError> {
        self.consume_error()?;
        let name = self.consume_identifier()?;
        self.consume_lbrace()?;

        let variants = self.parse_error_variant_list()?;

        self.consume_rbrace()?;

        Ok(ast::ErrorDecl { name, variants })
    }

    // function       → "func" IDENTIFIER genericArgs? "(" parameters? ")"
    //                  ( "->" type )?
    //                  block ;
    fn parse_function(&mut self) -> Result<ast::FunctionDecl, CompileError> {
        self.consume_func()?;
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
            ast::Type::Empty
        };

        let body = self.parse_block()?;

        Ok(ast::FunctionDecl {
            name,
            params,
            return_type,
            body,
            generic_params,
            is_self: false,
        })
    }

    // structDecl          → "struct" IDENTIFIER genericArgs? "{" structList? "}" ;
    fn parse_struct(&mut self) -> Result<ast::StructDecl, CompileError> {
        self.consume_struct()?;
        let name = self.consume_identifier()?;

        let generic_params = self.parse_optional_generic_params()?;

        self.consume_lbrace()?;
        let fields = if self.match_rbrace() {
            self.advance();
            Vec::new()
        } else {
            self.parse_struct_list()?
        };

        self.consume_rbrace()?;

        Ok(ast::StructDecl {
            name,
            fields,
            generic_params,
        })
    }

    // extensionDecl       → "extension" IDENTIFIER genericArgs? "{" funcDecl* "}" ;
    fn parse_extension(&mut self) -> Result<ast::ExtensionDecl, CompileError> {
        self.consume_extension()?;

        let name = self.consume_identifier()?;

        let generic_params = self.parse_optional_generic_params()?;

        self.consume_lbrace()?;

        let mut functions = Vec::new();
        while !self.match_rbrace() && !self.at_end() {
            functions.push(self.parse_self_permitting_func()?);
        }

        self.consume_rbrace()?;
        Ok(ast::ExtensionDecl {
            name,
            generic_params,
            functions,
        })
    }

    // selfPermFuncDecl    → "func" selfPermittingFunc ;
    // selfPermittingFunc → IDENTIFIER genericArgs? "(" "self"? ( "," parameters )? ")"
    //                      ( "->" type )?
    //                      block ;
    fn parse_self_permitting_func(&mut self) -> Result<ast::FunctionDecl, CompileError> {
        self.consume_func()?;
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
            ast::Type::Empty
        };

        let body = self.parse_block()?;

        Ok(ast::FunctionDecl {
            name,
            params,
            return_type,
            body,
            generic_params,
            is_self,
        })
    }
}
