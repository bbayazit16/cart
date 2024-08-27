use crate::ast;
use crate::errors::CompileError;
use crate::parser::Parser;
use crate::token::TokenType;

impl Parser {
    // statement      → exprStmt
    //                | letStmt
    //                | useStmt
    //                | returnStmt
    //                | forStmt
    //                | whileStmt
    //                | doWhileStmt ;
    pub(super) fn parse_stmt(&mut self) -> ast::Stmt {
        // self.parse_stmt_()
        match self.parse_stmt_() {
            Ok(stmt) => stmt,
            Err(e) => {
                self.errors.push(e);
                self.synchronize();
                ast::Stmt::NotRecovered
            }
        }
    }

    fn parse_stmt_(&mut self) -> Result<ast::Stmt, CompileError> {
        match self.peek()?.token_type {
            TokenType::Let => Ok(ast::Stmt::Let(self.parse_let_stmt()?)),
            TokenType::Use => Ok(self.parse_use_stmt()?),
            TokenType::Return => Ok(self.parse_return_stmt()?),
            TokenType::For => Ok(ast::Stmt::For(self.parse_for_stmt()?)),
            TokenType::While => Ok(ast::Stmt::While(self.parse_while_stmt()?)),
            TokenType::Do => Ok(ast::Stmt::DoWhile(self.parse_do_while_stmt()?)),
            _ => self.parse_expr_stmt(),
        }
    }

    // letStmt → "let" "mut"? IDENTIFIER ( ":" type )? "=" expression ";" ;
    fn parse_let_stmt(&mut self) -> Result<ast::LetStmt, CompileError> {
        let starting_span = self.consume_let()?.span;

        let is_mut = if self.match_mut() {
            self.advance();
            true
        } else {
            false
        };

        let name = self.consume_identifier()?;
        let set_type = if self.match_colon() {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        let eq_span = self.consume_equal()?.span;

        let expr = self.parse_expr();
        if matches!(expr, ast::Expr::NotRecovered) {
            return Ok(ast::LetStmt {
                span: starting_span.merge(&eq_span),
                name,
                is_mut,
                set_type,
                expr: ast::Expr::NotRecovered,
            });
        }

        let ending_span = self.consume_semicolon()?.span;

        Ok(ast::LetStmt {
            name,
            is_mut,
            set_type,
            expr,
            span: starting_span.merge(&ending_span),
        })
    }

    // useStmt        → "use" IDENTIFIER ( "::" IDENTIFIER )* ( "::" "*" )? ";" ;
    fn parse_use_stmt(&mut self) -> Result<ast::Stmt, CompileError> {
        self.consume_use()?;
        let mut path = vec![self.consume_identifier()?];
        while self.match_colon_colon() {
            self.advance();
            if self.match_star() {
                path.push(self.advance());
                break;
            } else {
                path.push(self.consume_identifier()?);
            }
        }
        self.consume_semicolon()?;
        Ok(ast::Stmt::Use(path))
    }

    // returnStmt     → "return" expression? ";" ;
    fn parse_return_stmt(&mut self) -> Result<ast::Stmt, CompileError> {
        let return_span = self.consume_return()?.span;
        let expr = if self.match_semicolon() {
            None
        } else {
            Some(self.parse_expr())
        };
        self.consume_semicolon()?;

        Ok(ast::Stmt::Return(return_span, expr))
    }

    // forStmt        → "for" ( IDENTIFIER | "_" ) "in" expression block ;
    fn parse_for_stmt(&mut self) -> Result<ast::ForStmt, CompileError> {
        let starting_span = self.consume_for()?.span;
        let iterator = if self.match_underscore() {
            self.advance();
            None
        } else {
            Some(self.consume_identifier()?)
        };

        self.consume_in()?;
        let iterable = self.parse_expr();
        let body = self.parse_block()?;

        Ok(ast::ForStmt {
            span: starting_span.merge(&body.span),
            iterator,
            iterable,
            body,
        })
    }

    // whileStmt      → "while" expression block ;
    fn parse_while_stmt(&mut self) -> Result<ast::WhileStmt, CompileError> {
        let starting_span = self.consume_while()?.span;
        let condition = self.parse_expr();
        let body = self.parse_block()?;
        Ok(ast::WhileStmt {
            span: starting_span.merge(&body.span),
            body,
            condition,
        })
    }

    // doWhileStmt    → "do" block "while" expression ";" ;
    fn parse_do_while_stmt(&mut self) -> Result<ast::DoWhileStmt, CompileError> {
        let starting_span = self.consume_do()?.span;
        let body = self.parse_block()?;
        self.consume_while()?;

        let condition = self.parse_expr();

        let ending_span = self.consume_semicolon()?.span;

        Ok(ast::DoWhileStmt {
            span: starting_span.merge(&ending_span),
            body,
            condition,
        })
    }

    // exprStmt       → expression ";" ;
    fn parse_expr_stmt(&mut self) -> Result<ast::Stmt, CompileError> {
        let expr = self.parse_expr();
        self.consume_semicolon()?;
        Ok(ast::Stmt::Expression(expr))
    }
}
