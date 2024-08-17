use crate::ast;
use crate::errors::CompileError;
use crate::parser::Parser;
use crate::token::Token;

impl Parser {
    // statement      → exprStmt
    //                | letStmt
    //                | useStmt
    //                | returnStmt
    //                | forStmt
    //                | whileStmt
    //                | doWhileStmt ;
    pub(super) fn parse_stmt(&mut self) -> Result<ast::Stmt, CompileError> {
        match self.peek()? {
            Token::Let(..) => Ok(ast::Stmt::Let(Box::new(self.parse_let_stmt()?))),
            Token::Use(..) => Ok(self.parse_use_stmt()?),
            Token::Return(..) => Ok(self.parse_return_stmt()?),
            Token::For(..) => Ok(ast::Stmt::For(Box::new(self.parse_for_stmt()?))),
            Token::While(..) => Ok(ast::Stmt::While(Box::new(self.parse_while_stmt()?))),
            Token::Do(..) => Ok(ast::Stmt::DoWhile(Box::new(self.parse_do_while_stmt()?))),
            _ => self.parse_expr_stmt(),
        }
    }

    // letStmt → "let" "mut"? IDENTIFIER ( ":" type )? "=" expression ";" ;
    fn parse_let_stmt(&mut self) -> Result<ast::LetStmt, CompileError> {
        self.consume_let()?;
        // if let Err(e) = self.consume_let() {
        //     self.errors.push(e);
        //     self.synchronize();
        //     dbg!(&self.peek());
        //     // return
        // }

        let is_mut = if self.match_mut() {
            self.advance();
            true
        } else {
            false
        };

        // let name = self.consume_identifier()?;
        let name = self.consume_identifier()?;
        let set_type = if self.match_colon() {
            self.advance();
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
        self.consume_return()?;
        let expr = if self.match_semicolon() {
            None
        } else {
            Some(self.parse_expr()?)
        };
        self.consume_semicolon()?;

        Ok(ast::Stmt::Return(expr))
    }

    // forStmt        → "for" ( IDENTIFIER | "_" ) "in" expression block ;
    fn parse_for_stmt(&mut self) -> Result<ast::ForStmt, CompileError> {
        self.consume_for()?;
        let iterator = if self.match_underscore() {
            self.advance();
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

    // exprStmt       → expression ";" ;
    fn parse_expr_stmt(&mut self) -> Result<ast::Stmt, CompileError> {
        let expr = self.parse_expr()?;
        self.consume_semicolon()?;
        Ok(ast::Stmt::Expression(Box::new(expr)))
    }
}
