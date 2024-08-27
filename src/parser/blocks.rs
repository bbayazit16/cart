use crate::ast;
use crate::errors::CompileError;
use crate::parser::Parser;

impl Parser {
    // block          → "{" declaration* expression? "}" ;
    pub(super) fn parse_block(&mut self) -> Result<ast::Block, CompileError> {
        let starting_span = self.consume_lbrace()?.span;

        let mut declarations = Vec::new();
        let mut return_expr: Option<ast::Expr> = None;
        while !self.match_rbrace() && !self.at_end() {
            let declaration_start = self.peek()?.span;
            let declaration = self.parse_declaration();
            
            if matches!(declaration, ast::Declaration::StatementDecl(ast::Stmt::NotRecovered)) {
                let last_err = self.errors.pop().unwrap();
                self.recover_to_position(declaration_start.start)?;
                let parsed_expr = self.parse_expr();
                if matches!(parsed_expr, ast::Expr::NotRecovered) {
                    self.errors.pop();
                    self.errors.push(last_err);
                    break;
                } else {
                    return_expr = Some(parsed_expr);
                    break;
                }
            } else {
                declarations.push(declaration)
            }
        }
        
        let ending_span = self.consume_rbrace()?.span;

        Ok(ast::Block {
            declarations,
            return_expr: return_expr.map(Box::new),
            span: starting_span.merge(&ending_span),
        })
    }
}
