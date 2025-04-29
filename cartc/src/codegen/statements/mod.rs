mod let_statement;

use crate::codegen::CodeGen;
use crate::hir::Statement;

impl CodeGen<'_> {
    // /// Generates the LLVM IR for a statement.
    pub(crate) fn generate_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Expression(ref expr) => {
                // 'Expression statements' happen only during assignment, which
                // return the r value.
                self.generate_expression_r_value(expr);
            }
            Statement::Let {
                ref name,
                ref value,
                .. // ty is unused
            } => {
                self.generate_let_stmt(name, value);
            }
        }
    }
}
