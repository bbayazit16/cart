use crate::ast;
use crate::ast::not_recovered::NotRecovered;
use crate::errors::CompileError;
use crate::parser::Parser;

impl Parser {
    // expression     → assignment ;
    pub(super) fn parse_expr(&mut self) -> Result<ast::Expr, CompileError> {
        self.parse_assignment()
    }

    // assignment     → ( ( ifExpr "." ) | ( matchExpr "." ) | ( call "." ) )?
    //                  IDENTIFIER "=" expression
    //                | ifExpr ;
    fn parse_assignment(&mut self) -> Result<ast::Expr, CompileError> {
        // Assume expr is an r-value
        let expr = self.parse_if_expr()?;

        if self.match_equal() {
            // Assignment - convert from r-value to l-value
            self.advance();
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
            self.advance();

            let condition = Box::new(self.parse_expr()?);
            let then_branch = Box::new(self.parse_block()?);

            let mut elif_branches = Vec::new();
            while self.match_elif() {
                self.advance();
                let elif_condition = self.parse_expr()?;
                let block = self.parse_block()?;
                elif_branches.push((elif_condition, block));
            }

            let else_branch = if self.match_else() {
                self.advance();
                Some(Box::new(self.parse_block()?))
            } else {
                None
            };

            Ok(ast::Expr::If(Box::new(ast::IfExpr {
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
            self.advance();
            let value = Box::new(self.parse_expr()?);

            // TODO EOF
            self.consume_lbrace()?;
            let arms = if self.match_rbrace() {
                Vec::new()
            } else {
                self.parse_match_arm_list()?
            };
            self.consume_rbrace()?;

            Ok(ast::Expr::Match(Box::new(ast::MatchExpr {
                value,
                arms,
            })))
        } else {
            self.parse_logical_or()
        }
    }

    // logicalOr      → logicalAnd ( "||" logicalAnd )* ;
    fn parse_logical_or(&mut self) -> Result<ast::Expr, CompileError> {
        let mut left = self.parse_logical_and()?;
        while self.match_pipe_pipe() {
            let operator = self.advance();
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
            let operator = self.advance();
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
            let operator = self.advance();
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
            let operator = self.advance();
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
            let operator = self.advance();
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
            let operator = self.advance();
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
            let operator = self.advance();
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
            self.advance();
            ast::Expr::StructAccess(Box::new(self.partial_parse_struct_access(primary)?))
        } else {
            primary
        };

        if self.match_lparen() {
            self.advance();
            let arguments = if self.match_rparen() {
                Vec::new()
            } else {
                self.parse_arguments()?
            };
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
            Ok(ast::Expr::Literal(ast::Literal::Bool(self.advance())))
        } else if self.match_number() {
            // NUMBER
            Ok(ast::Expr::Literal(ast::Literal::Number(self.advance())))
        } else if self.match_string() {
            // STRING
            Ok(ast::Expr::Literal(ast::Literal::String(self.advance())))
        } else if self.match_lparen() {
            // "(" expression ")"
            self.advance();
            let expr = self.parse_expr()?;
            self.consume_rparen()?;
            Ok(expr)
        } else if self.match_lbrace() {
            // block
            Ok(ast::Expr::Block(Box::new(self.parse_block()?)))
        } else {
            // if self.match_lbrace() {
            //     // structLiteral
            //     let struct_value = self.parse_struct_literal(None)?;
            //     return Ok(ast::Expr::StructLiteral(Box::new(struct_value)));
            // };

            let ident = self.consume_identifier()?;
            // structLiteral | enumValue | IDENTIFIER | IDENTIFIER ( "." IDENTIFIER )+
            if self.match_colon_colon() {
                // enumValue
                let enum_value = self.parse_enum_value(Some(ident))?;
                Ok(ast::Expr::EnumValue(Box::new(enum_value)))
            } else if self.match_lbrace() {
                // structLiteral
                let ident_position = ident.get_file_pointer();
                let struct_or_ident = self.parse_struct_literal(Some(ident));
                match struct_or_ident {
                    Ok(struct_value) if !struct_value.is_not_recovered() => {
                        Ok(ast::Expr::StructLiteral(Box::new(struct_value)))
                    }
                    _ => {
                        // Then, something like:
                        // if value { ... }
                        // and `value { ... }` is being attempted to parse
                        // as a struct.
                        self.recover_to_position(ident_position)?;
                        let ident = self.advance();
                        Ok(ast::Expr::Variable(ident, None))
                    }
                }
            } else if self.match_dot() {
                self.advance();
                // TODO: Reporter - temp. variable
                // TODO: Handle expr properly
                // IDENTIFIER ( "." IDENTIFIER )+
                Ok(ast::Expr::StructAccess(Box::new(
                    self.partial_parse_struct_access(ast::Expr::Variable(ident, None))?,
                )))
            } else {
                // IDENTIFIER
                Ok(ast::Expr::Variable(ident, None))
            }
        }
    }
}