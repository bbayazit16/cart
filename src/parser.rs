use crate::ast;
use crate::error::CompileError;
use crate::lexer::Lexer;
use crate::reporter::Reporter;
use crate::token::Token;
use std::collections::VecDeque;

macro_rules! generate_consume_impl {
    ($($name:ident => $pattern:pat, $expected:expr),+ $(,)?) => {
        use crate::error::SyntaxError;
        impl Parser {
            $(
                fn $name(&mut self) -> Result<Token, CompileError> {
                    let token = self.advance()?;
                    match token {
                        $pattern => Ok(token),
                        _ => {
                            let e = CompileError::Syntax(SyntaxError::ExpectedDifferentCharacter {
                                file_position: token.get_file_position(),
                                line: token.get_line(),
                                line_position: token.get_line_position(),
                                expected: $expected.to_string()
                            });
                            self.reporter.report(&e);
                            Err(e)
                        }
                    }
                }
            )+
        }
    };
}

macro_rules! generate_match_impl {
    ($($name:ident => $pattern:pat),+ $(,)?) => {
        impl Parser {
            $(
                fn $name(&mut self) -> bool {
                    let token = match self.peek() {
                        Ok(token) => token,
                        Err(_) => return false,
                    };
                    matches!(token, $pattern)
                }
            )+
        }
    };
}

pub struct Parser {
    lexer: Lexer,
    reporter: Reporter,
    token_queue: VecDeque<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer, reporter: Reporter) -> Self {
        Parser {
            lexer,
            reporter,
            token_queue: VecDeque::new(),
        }
    }

    /// Parses the entire program, returning CompileError or
    /// the output Program, a vector of declaration ASTs.
    pub fn parse(&mut self) -> Result<ast::Program, CompileError> {
        let mut declarations = Vec::new();
        while !(self.is_at_end() || self.match_eof()) {
            match self.parse_declaration() {
                Ok(declaration) => declarations.push(declaration),
                Err(e) => return Err(e),
            }
            self.clear_queue();
        }
        Ok(ast::Program { declarations })
    }

    /// Parse and return a declaration.
    fn parse_declaration(&mut self) -> Result<ast::Declaration, CompileError> {
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

    // statement      → exprStmt
    //                | letStmt
    //                | useStmt
    //                | returnStmt
    //                | forStmt
    //                | whileStmt
    //                | doWhileStmt ;
    fn parse_stmt(&mut self) -> Result<ast::Stmt, CompileError> {
        match self.peek()? {
            Token::Let(..) => Ok(ast::Stmt::Let(Box::new(self.parse_let_stmt()?))),
            Token::Use(..) => self.parse_use_stmt(),
            Token::Return(..) => self.parse_return_stmt(),
            Token::For(..) => Ok(ast::Stmt::For(Box::new(self.parse_for_stmt()?))),
            Token::While(..) => Ok(ast::Stmt::While(Box::new(self.parse_while_stmt()?))),
            Token::Do(..) => Ok(ast::Stmt::DoWhile(Box::new(self.parse_do_while_stmt()?))),
            _ => self.parse_expr_stmt(),
        }
    }

    // exprStmt       → expression ";" ;
    fn parse_expr_stmt(&mut self) -> Result<ast::Stmt, CompileError> {
        let expr = self.parse_expr()?;
        self.consume_semicolon()?;
        Ok(ast::Stmt::Expression(Box::new(expr)))
    }

    // doWhileStmt    → "do" block "while" expression ";" ;
    fn parse_do_while_stmt(&mut self) -> Result<ast::DoWhileStmt, CompileError> {
        self.consume_do()?;
        let body = self.parse_block()?;
        self.consume_while()?;

        let condition = self.parse_expr()?;

        self.consume_semicolon()?;

        Ok(ast::DoWhileStmt {
            body: Box::new(body),
            condition,
        })
    }

    // whileStmt      → "while" expression block ;
    fn parse_while_stmt(&mut self) -> Result<ast::WhileStmt, CompileError> {
        self.consume_while()?;
        let condition = self.parse_expr()?;
        let body = self.parse_block()?;
        Ok(ast::WhileStmt {
            body: Box::new(body),
            condition,
        })
    }

    // forStmt        → "for" ( IDENTIFIER | "_" ) "in" expression block ;
    fn parse_for_stmt(&mut self) -> Result<ast::ForStmt, CompileError> {
        self.consume_for()?;
        let iterator = if self.match_underscore() {
            self.advance()?;
            None
        } else {
            Some(self.consume_identifier()?)
        };

        self.consume_in()?;
        let iterable = self.parse_expr()?;
        let body = self.parse_block()?;

        Ok(ast::ForStmt {
            iterator,
            iterable,
            body: Box::new(body),
        })
    }

    // returnStmt     → "return" expression? ";" ;
    fn parse_return_stmt(&mut self) -> Result<ast::Stmt, CompileError> {
        self.consume_return()?;
        let expr = if self.match_semicolon() {
            None
        } else {
            Some(self.parse_expr()?)
        };
        self.consume_semicolon()?;

        Ok(ast::Stmt::Return(expr))
    }

    // useStmt        → "use" IDENTIFIER ( "::" IDENTIFIER )* ( "::" "*" )? ";" ;
    fn parse_use_stmt(&mut self) -> Result<ast::Stmt, CompileError> {
        self.consume_use()?;
        let mut path = vec![self.consume_identifier()?];
        while self.match_colon_colon() {
            self.advance()?;
            if self.match_star() {
                path.push(self.advance()?);
                break;
            } else {
                path.push(self.consume_identifier()?);
            }
        }
        self.consume_semicolon()?;
        Ok(ast::Stmt::Use(path))
    }

    // letStmt → "let" "mut"? IDENTIFIER ( ":" type )? "=" expression ";" ;
    fn parse_let_stmt(&mut self) -> Result<ast::LetStmt, CompileError> {
        self.consume_let()?;
        let is_mut = if self.match_mut() {
            self.advance()?;
            true
        } else {
            false
        };

        let name = self.consume_identifier()?;
        let set_type = if self.match_colon() {
            self.advance()?;
            Some(self.parse_type()?)
        } else {
            None
        };

        self.consume_equal()?;

        let expr = self.parse_expr()?;

        self.consume_semicolon()?;

        Ok(ast::LetStmt {
            name,
            is_mut,
            set_type,
            expr,
        })
    }

    fn parse_expr(&mut self) -> Result<ast::Expr, CompileError> {
        self.parse_assignment()
    }

    // expression     → assignment
    //
    // assignment     → ( ( ifExpr "." ) | ( matchExpr "." ) | ( call "." ) )?
    //                  IDENTIFIER "=" expression
    //                | ifExpr ;
    fn parse_assignment(&mut self) -> Result<ast::Expr, CompileError> {
        // Assume expr is an r-value
        let expr = self.parse_if_expr()?;

        if self.match_equal() {
            // Assignment - convert from r-value to l-value
            self.advance()?;
            // TODO: check parse_expr, could replace parse_expr with
            // parse_if_expr, and skip parse_assignment later on
            let r_value = Box::new(self.parse_expr()?);
            Ok(ast::Expr::Assignment(Box::new(ast::AssignmentExpr {
                l_value: Box::new(expr),
                r_value,
            })))
        } else {
            Ok(expr)
        }
    }

    // ifExpr         → "if" expression block
    //                  ( "elif" expression block )*
    //                  ( "else" block )?
    //                | matchExpr ;
    fn parse_if_expr(&mut self) -> Result<ast::Expr, CompileError> {
        if self.match_if() {
            self.advance()?;
            
            let condition = Box::new(self.parse_expr()?);
            let then_branch = Box::new(self.parse_block()?);

            let mut elif_branches = Vec::new();
            while self.match_elif() {
                self.advance()?;
                let elif_condition = self.parse_expr()?;
                let block = self.parse_block()?;
                elif_branches.push((elif_condition, block));
            }

            let else_branch = if self.match_else() {
                self.advance()?;
                Some(Box::new(self.parse_block()?))
            } else {
                None
            };

            Ok(ast::Expr::IfExpr(Box::new(ast::IfExpr {
                condition,
                then_branch,
                elif_branches,
                else_branch,
            })))
        } else {
            self.parse_match_expr()
        }
    }

    // matchExpr      → "match" expression "{" matchArmList? "}"
    //                | logicalOr ;
    fn parse_match_expr(&mut self) -> Result<ast::Expr, CompileError> {
        if self.match_match() {
            self.advance()?;
            let value = Box::new(self.parse_expr()?);

            // TODO EOF
            self.consume_lbrace()?;
            let arms = if self.match_rbrace() {
                Vec::new()
            } else {
                self.parse_match_arm_list()?
            };
            self.consume_rbrace()?;

            Ok(ast::Expr::MatchExpr(Box::new(ast::MatchExpr {
                value,
                arms,
            })))
        } else {
            self.parse_logical_or()
        }
    }

    // matchArmList   → matchArm ( "," matchArm )*
    fn parse_match_arm_list(&mut self) -> Result<Vec<ast::MatchArm>, CompileError> {
        let mut arms = vec![self.parse_match_arm()?];
        while self.match_comma() {
            self.advance()?;
            arms.push(self.parse_match_arm()?);
        }
        Ok(arms)
    }

    // matchArm       → pattern "=>" expression ;
    fn parse_match_arm(&mut self) -> Result<ast::MatchArm, CompileError> {
        let pattern = self.parse_pattern()?;

        self.consume_fat_arrow()?;

        let body = Box::new(self.parse_expr()?);

        Ok(ast::MatchArm { pattern, body })
    }

    // pattern        → IDENTIFIER ( ( "(" identList ")" )? | ( "{" identList "}" ) )
    //                | "_" ;
    fn parse_pattern(&mut self) -> Result<ast::Pattern, CompileError> {
        if self.match_underscore() {
            self.advance()?;
            Ok(ast::Pattern::Wildcard)
        } else {
            let ident = self.consume_identifier()?;
            if self.match_lparen() {
                self.advance()?;
                let ident_list = self.parse_ident_list()?;
                self.consume_rparen()?;
                Ok(ast::Pattern::EnumOrStructVariant(
                    ast::PatternType::Enum,
                    ident,
                    ident_list,
                ))
            } else if self.match_lbrace() {
                self.advance()?;
                let ident_list = self.parse_ident_list()?;
                self.consume_rbrace()?;
                Ok(ast::Pattern::EnumOrStructVariant(
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
            self.advance()?;
            ident_list.push(self.consume_identifier()?);
        }
        Ok(ident_list)
    }

    // logicalOr      → logicalAnd ( "||" logicalAnd )* ;
    fn parse_logical_or(&mut self) -> Result<ast::Expr, CompileError> {
        let mut left = self.parse_logical_and()?;
        while self.match_pipe_pipe() {
            let operator = self.advance()?;
            let right = Box::new(self.parse_logical_and()?);
            left = ast::Expr::Binary(Box::new(ast::BinaryExpr {
                left: Box::new(left),
                operator,
                right,
            }));
        }
        Ok(left)
    }

    // logicalAnd     → equality ( "&&" equality )* ;
    fn parse_logical_and(&mut self) -> Result<ast::Expr, CompileError> {
        let mut left = self.parse_equality()?;
        while self.match_ampersand_ampersand() {
            let operator = self.advance()?;
            let right = Box::new(self.parse_equality()?);
            left = ast::Expr::Binary(Box::new(ast::BinaryExpr {
                left: Box::new(left),
                operator,
                right,
            }));
        }
        Ok(left)
    }

    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    fn parse_equality(&mut self) -> Result<ast::Expr, CompileError> {
        let mut left = self.parse_comparison()?;
        while self.match_bang_equal() || self.match_equal_equal() {
            let operator = self.advance()?;
            let right = Box::new(self.parse_comparison()?);
            left = ast::Expr::Binary(Box::new(ast::BinaryExpr {
                left: Box::new(left),
                operator,
                right,
            }));
        }
        Ok(left)
    }

    // comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn parse_comparison(&mut self) -> Result<ast::Expr, CompileError> {
        let mut left = self.parse_term()?;
        while self.match_ge() || self.match_geq() || self.match_le() || self.match_leq() {
            let operator = self.advance()?;
            let right = Box::new(self.parse_term()?);
            left = ast::Expr::Binary(Box::new(ast::BinaryExpr {
                left: Box::new(left),
                operator,
                right,
            }));
        }
        Ok(left)
    }

    // term           → factor ( ( "-" | "+" ) factor )* ;
    fn parse_term(&mut self) -> Result<ast::Expr, CompileError> {
        let mut left = self.parse_factor()?;
        while self.match_minus() || self.match_plus() {
            let operator = self.advance()?;
            let right = Box::new(self.parse_factor()?);
            left = ast::Expr::Binary(Box::new(ast::BinaryExpr {
                left: Box::new(left),
                operator,
                right,
            }));
        }
        Ok(left)
    }

    // factor         → unary ( ( "/" | "*" ) unary )* ;
    fn parse_factor(&mut self) -> Result<ast::Expr, CompileError> {
        let mut left = self.parse_unary()?;
        while self.match_slash() || self.match_star() {
            let operator = self.advance()?;
            let right = Box::new(self.parse_unary()?);
            left = ast::Expr::Binary(Box::new(ast::BinaryExpr {
                left: Box::new(left),
                operator,
                right,
            }));
        }
        Ok(left)
    }

    // unary          → ( "!" | "-" ) unary | call ;
    fn parse_unary(&mut self) -> Result<ast::Expr, CompileError> {
        if self.match_bang() || self.match_minus() {
            let operator = self.advance()?;
            let right = self.parse_unary()?;
            Ok(ast::Expr::Unary(Box::new(ast::UnaryExpr {
                operator,
                right,
            })))
        } else {
            self.parse_call()
        }
    }

    // call           → primary ( "." IDENTIFIER )* "(" arguments? ")"
    //                | primary ;
    fn parse_call(&mut self) -> Result<ast::Expr, CompileError> {
        let primary = self.parse_primary()?;
        // TODO: probably not necessary, already delegated to primary above
        let callee = if self.match_dot() {
            self.advance()?;
            ast::Expr::StructAccess(Box::new(self.partial_parse_struct_access(primary)?))
        } else {
            primary
        };

        if self.match_lparen() {
            self.advance()?;
            let arguments = self.parse_arguments()?;
            self.consume_rparen()?;
            Ok(ast::Expr::Call(Box::new(ast::CallExpr {
                callee,
                arguments,
            })))
        } else {
            // Directly return callee
            // That is the original `primary`, but moved.
            Ok(callee)
        }
    }

    // primary        → "true" | "false"
    //                | NUMBER | STRING | IDENTIFIER | "(" expression ")"
    //                | block | structLiteral | enumValue
    //                | IDENTIFIER ( "." IDENTIFIER )+ ;
    fn parse_primary(&mut self) -> Result<ast::Expr, CompileError> {
        if self.match_bool() {
            // "true" | "false"
            Ok(ast::Expr::Literal(ast::Literal::Bool(self.advance()?)))
        } else if self.match_number() {
            // NUMBER
            Ok(ast::Expr::Literal(ast::Literal::Number(self.advance()?)))
        } else if self.match_string() {
            // STRING
            Ok(ast::Expr::Literal(ast::Literal::String(self.advance()?)))
        } else if self.match_lparen() {
            // "(" expression ")"
            self.advance()?;
            let expr = self.parse_expr()?;
            self.consume_rparen()?;
            Ok(expr)
        } else if self.match_lbrace() {
            // block
            Ok(ast::Expr::Block(Box::new(self.parse_block()?)))
        } else {
            if self.match_lbrace() {
                // structLiteral
                let struct_value = self.parse_struct_literal(None)?;
                return Ok(ast::Expr::StructLiteral(Box::new(struct_value)));
            };

            let ident = self.consume_identifier()?;
            // structLiteral | enumValue | IDENTIFIER | IDENTIFIER ( "." IDENTIFIER )+
            if self.match_colon_colon() {
                // enumValue
                let enum_value = self.parse_enum_value(Some(ident))?;
                Ok(ast::Expr::EnumValue(Box::new(enum_value)))
            } else if self.match_lbrace() {
                // structLiteral
                let struct_value = self.parse_struct_literal(Some(ident))?;
                Ok(ast::Expr::StructLiteral(Box::new(struct_value)))
            } else if self.match_dot() {
                self.advance()?;
                // TODO: Reporter - temp. variable
                // TODO: Handle expr properly
                // IDENTIFIER ( "." IDENTIFIER )+
                Ok(ast::Expr::StructAccess(Box::new(
                    self.partial_parse_struct_access(ast::Expr::Variable(ident))?,
                )))
            } else {
                // IDENTIFIER
                Ok(ast::Expr::Variable(ident))
            }
        }
    }

    fn partial_parse_struct_access(
        &mut self,
        object: ast::Expr,
    ) -> Result<ast::StructAccessExpr, CompileError> {
        // TODO: Reporter - temp. variable
        // TODO: Handle expr properly
        let mut fields = vec![self.consume_identifier()?];
        while self.match_dot() {
            self.advance()?;
            fields.push(self.consume_identifier()?);
        }

        Ok(ast::StructAccessExpr { object, fields })
    }

    // structLiteral  → IDENTIFIER "{" structParamValues? "}" ;
    fn parse_struct_literal(
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
            self.advance()?;
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

    // enumValue      → IDENTIFIER ( "::" IDENTIFIER )? ( "(" arguments? ")" )? ;
    fn parse_enum_value(
        &mut self,
        existing_name: Option<Token>,
    ) -> Result<ast::EnumValue, CompileError> {
        let name = match existing_name {
            Some(exn) => exn,
            None => self.consume_identifier()?,
        };

        let variant = if self.match_colon_colon() {
            self.advance()?;
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
        self.advance()?;

        let arguments = if self.match_rparen() {
            // Directly closed, i.e, Enum::Ident()
            // Has no effect; the same as Enum::Ident
            Vec::new()
        } else {
            self.parse_arguments()?
        };

        self.advance()?;

        Ok(ast::EnumValue {
            name,
            variant,
            arguments,
        })
    }

    // arguments      → expression ( "," expression )* ;
    fn parse_arguments(&mut self) -> Result<Vec<ast::Expr>, CompileError> {
        let mut arguments = vec![self.parse_expr()?];
        while self.match_comma() {
            self.advance()?;
            arguments.push(self.parse_expr()?);
        }
        Ok(arguments)
    }

    // extensionDecl       → "extension" IDENTIFIER genericArgs? "{" function* "}" ;
    fn parse_extension(&mut self) -> Result<ast::ExtensionDecl, CompileError> {
        self.consume_extension()?;

        let name = self.consume_identifier()?;

        let generic_params = self.parse_optional_generic_params()?;

        self.consume_lbrace()?;

        let mut functions = Vec::new();
        while !self.match_rbrace() {
            functions.push(self.parse_function()?);
        }

        self.consume_rbrace()?;
        Ok(ast::ExtensionDecl {
            name,
            generic_params,
            functions,
        })
    }

    // structDecl          → "struct" IDENTIFIER genericArgs? "{" structList? "}" ;
    fn parse_struct(&mut self) -> Result<ast::StructDecl, CompileError> {
        self.consume_struct()?;
        let name = self.consume_identifier()?;

        let generic_params = self.parse_optional_generic_params()?;

        self.consume_lbrace()?;
        let fields = if self.match_rbrace() {
            self.advance()?;
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
            self.advance()?;
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
        })
    }

    /// Checks for '<', and returns Vec<Type> of generic params.
    /// Consumes both '<' and '>'.
    /// If there are no generic types, returns an empty vector
    /// without consuming anything.
    fn parse_optional_generic_params(&mut self) -> Result<Vec<ast::Type>, CompileError> {
        if self.match_langle() {
            self.advance()?;
            let generic_list = self.parse_typelist()?;
            self.consume_rangle()?;
            Ok(generic_list)
        } else {
            Ok(Vec::new())
        }
    }

    // block          → "{" declaration* expression? "}" ;
    // TODO: last expression is return value
    fn parse_block(&mut self) -> Result<ast::Block, CompileError> {
        self.consume_lbrace()?;

        let mut declarations = Vec::new();
        while !self.match_rbrace() {
            declarations.push(self.parse_declaration()?);
        }

        self.consume_rbrace()?;

        // TODO: Handle return expr.
        Ok(ast::Block {
            declarations,
            return_expr: None,
        })
    }

    // parameters     → parameter ( "," parameter )* ;
    fn parse_params(&mut self) -> Result<Vec<ast::Parameter>, CompileError> {
        let mut params = vec![self.parse_param()?];

        while self.match_comma() {
            self.advance()?;
            params.push(self.parse_param()?);
        }

        Ok(params)
    }

    // parameter      → IDENTIFIER ":" type ;
    fn parse_param(&mut self) -> Result<ast::Parameter, CompileError> {
        let name = self.consume_identifier()?;
        self.consume_colon()?;
        let param_type = self.parse_type()?;

        Ok(ast::Parameter { name, param_type })
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

    // errorVariantList    → errorVariant ( "," errorVariant )* ;
    fn parse_error_variant_list(&mut self) -> Result<Vec<ast::ErrorVariant>, CompileError> {
        let mut variants = Vec::new();
        if self.match_rbrace() {
            self.advance()?;
            return Ok(variants);
        } else {
            variants.push(self.parse_error_variant()?);
        }

        while !self.match_rbrace() {
            self.consume_comma()?;
            variants.push(self.parse_error_variant()?);
        }

        Ok(variants)
    }

    // errorVariant  → IDENTIFIER ( "(" structList ")" )? "=" STRING_LITERAL ;
    fn parse_error_variant(&mut self) -> Result<ast::ErrorVariant, CompileError> {
        let name = self.consume_identifier()?;
        let fields = if self.match_lparen() {
            self.advance()?;
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

    // enumVariantList     → enumVariant ( "," enumVariant )* ;
    fn parse_enum_variant_list(&mut self) -> Result<Vec<ast::EnumVariant>, CompileError> {
        let mut variants = Vec::new();
        if self.match_rbrace() {
            self.advance()?;
            return Ok(variants);
        } else {
            variants.push(self.parse_enum_variant()?);
        }

        while !self.match_rbrace() {
            self.consume_comma()?;
            variants.push(self.parse_enum_variant()?);
        }

        Ok(variants)
    }

    // enumVariant    → IDENTIFIER ( "(" enumTypeList ")" )?
    //                | IDENTIFIER ( "{" structList "}" ) ;
    fn parse_enum_variant(&mut self) -> Result<ast::EnumVariant, CompileError> {
        let identifier = self.consume_identifier()?;
        if self.match_lparen() {
            self.advance()?;
            // Then expect an enum typelist
            let typelist = self.parse_typelist()?;
            self.consume_rparen()?;

            Ok(ast::EnumVariant::Tuple(identifier, typelist))
        } else if self.match_lbrace() {
            self.advance()?;
            let struct_list = self.parse_struct_list()?;
            self.consume_rbrace()?;

            Ok(ast::EnumVariant::Struct(identifier, struct_list))
        } else {
            Ok(ast::EnumVariant::Simple(identifier))
        }
    }

    // enumTypeList   → type ( "," type )*
    fn parse_typelist(&mut self) -> Result<Vec<ast::Type>, CompileError> {
        let mut types = vec![self.parse_type()?];

        while self.match_comma() {
            self.advance()?;
            types.push(self.parse_type()?);
        }

        Ok(types)
    }

    // structList     → structField ( "," structField )*
    fn parse_struct_list(&mut self) -> Result<Vec<ast::StructField>, CompileError> {
        let mut struct_list = vec![self.parse_struct_field()?];

        // Do not change this to !self.match_rbrace.
        // In addition to parsing structs with braces, this function can
        // be used to parse errors, which contain structFields wrapped
        // around parens such as SomeError(x: type).
        while self.match_comma() {
            self.advance()?;
            struct_list.push(self.parse_struct_field()?);
        }

        Ok(struct_list)
    }

    // structField    → IDENTIFIER ":" type ;
    fn parse_struct_field(&mut self) -> Result<ast::StructField, CompileError> {
        let identifier = self.consume_identifier()?;
        self.consume_colon()?;
        let field_type = self.parse_type()?;

        Ok(ast::StructField {
            name: identifier,
            field_type,
        })
    }

    // type           → IDENTIFIER genericArgs?
    //                | "(" type? ")"
    fn parse_type(&mut self) -> Result<ast::Type, CompileError> {
        if self.match_lparen() {
            self.advance()?;
            if self.match_rparen() {
                self.advance()?;
                // If (), return empty type
                Ok(ast::Type::Empty)
            } else {
                // Return the inner type from the nested type
                let inner_type = self.parse_type()?;
                self.consume_rparen()?;
                Ok(inner_type)
            }
        } else {
            // Then, either IDENTIFIER genericArgs? OR type "|" type
            let identifier = self.consume_identifier()?;
            if self.match_langle() {
                // IDENTIFIER genericsArgs
                self.advance()?;
                let generic_list = self.parse_typelist()?;
                self.consume_rangle()?;
                Ok(ast::Type::Generic(identifier, generic_list))

                // Ok(type_)
                // if self.match_pipe() {
                //     self.advance()?;
                //     let right_type = self.parse_type()?;
                //     Ok(Type::Union(Box::new(type_), Box::new(right_type)))
                // } else {
                //     Ok(type_)
                // }
            } else {
                Ok(ast::Type::Simple(identifier))
                // Simply IDENTIFIER
                // let type_ = Type::Simple(identifier);
                //
                // // Check for union type
                // if self.match_pipe() {
                //     let right_type = self.parse_type()?;
                //     Ok(Type::Union(Box::new(type_), Box::new(right_type)))
                // } else {
                //     Ok(type_)
                // }
            }
        }
    }

    // fn check(&mut self, expected: Token) -> bool {
    //     if self.is_at_end() {
    //         false
    //     } else {
    //         std::mem::discriminant(self.peek().unwrap()) == std::mem::discriminant(&expected)
    //     }
    // }

    /// Advances the parser, consuming and returning the next token.
    fn advance(&mut self) -> Result<Token, CompileError> {
        if let Some(token) = self.token_queue.pop_front() {
            Ok(token)
        } else {
            self.lexer.request_next_token()
        }
    }

    /// Peeks the next token without consuming it.
    fn peek(&mut self) -> Result<&Token, CompileError> {
        if self.token_queue.is_empty() {
            let token = self.lexer.request_next_token()?;
            self.token_queue.push_back(token);
        }
        Ok(self.token_queue.front().unwrap())
    }

    /// Delegates is_at_end function to the lexer.
    fn is_at_end(&self) -> bool {
        self.token_queue.is_empty() && self.lexer.is_at_end()
    }

    /// Clears the token queue.
    fn clear_queue(&mut self) {
        self.token_queue.clear();
    }
}

generate_consume_impl! {
    consume_identifier => Token::Identifier(..), "an identifier",
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
    consume_fat_arrow => Token::FatArrow(..), "=>"
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
}
