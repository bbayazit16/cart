use crate::ast;
use crate::ast::not_recovered::NotRecovered;
use crate::errors::CompileError;
use crate::parser::Parser;

impl Parser {
    // block          → "{" declaration* expression? "}" ;
    pub(super) fn parse_block(&mut self) -> Result<ast::Block, CompileError> {
        self.consume_lbrace()?;

        let mut declarations = Vec::new();
        let mut return_expr: Option<ast::Expr> = None;
        while !self.match_rbrace() && !self.at_end() {
            let declaration_start = self.peek()?.get_file_pointer();
            let parsed_decl = self.parse_declaration();
            match parsed_decl {
                Ok(declaration) if !declaration.is_not_recovered() => {
                    declarations.push(declaration)
                }
                _ => {
                    self.recover_to_position(declaration_start)?;
                    return_expr = Some(self.parse_expr()?);
                    break;
                    //     self.errors.push(e);
                    //     dbg!(self.peek().unwrap());
                    //     // ast::NotRecovered::not_recovered()
                    //     // self.recover_to_position(declaration_start)?;
                    //     self.synchronize();
                    //     dbg!(self.peek().unwrap());
                    //     break;
                }
            }
            // declarations.push(self.parse_declaration()?);
        }

        self.consume_rbrace()?;

        // TODO: Handle return expr.
        Ok(ast::Block {
            declarations,
            return_expr: return_expr.map(Box::new),
        })
    }
}
