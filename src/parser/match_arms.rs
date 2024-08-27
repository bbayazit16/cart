use crate::ast;
use crate::errors::CompileError;
use crate::parser::Parser;

impl Parser {
    // matchArmList   → matchArm ( "," matchArm )*
    pub(super) fn parse_match_arm_list(&mut self) -> Result<Vec<ast::MatchArm>, CompileError> {
        let mut arms = vec![self.parse_match_arm()?];
        while self.match_comma() {
            self.advance();
            arms.push(self.parse_match_arm()?);
        }
        Ok(arms)
    }

    // matchArm       → pattern "=>" expression ;
    fn parse_match_arm(&mut self) -> Result<ast::MatchArm, CompileError> {
        let pattern = self.parse_pattern()?;

        self.consume_fat_arrow()?;

        let body = self.parse_expr();

        Ok(ast::MatchArm {
            span: pattern.span().merge(&body.span()),
            pattern,
            body,
        })
    }
}
