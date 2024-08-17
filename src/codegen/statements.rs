use crate::ast::{LetStmt, Stmt};
use crate::codegen::symbol_table::Variable;
use crate::codegen::CodeGen;
use crate::token_value;

impl<'ctx> CodeGen<'ctx> {
    /// Generates the LLVM IR for a statement.
    pub(super) fn generate_statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expression(expr) => {
                self.generate_expression(expr);
            }
            Stmt::Return(_) => unreachable!("Return statement"),
            // Stmt::Return(ref expr) => {
            //     self.generate_return(expr);
            // }
            Stmt::Let(ref let_stmt) => {
                self.generate_let_stmt(let_stmt);
            }
            _ => todo!(),
        }
    }

    /// Generates LLVM IR for let statements.
    fn generate_let_stmt(&mut self, let_stmt: &LetStmt) {
        let value = self.generate_expression(&let_stmt.expr).unwrap();
        let name = token_value!(&let_stmt.name);
        if let_stmt.is_mut {
            let alloca = self
                .builder
                .build_alloca(value.get_type(), &name)
                .expect("Failed to allocate variable");
            // entry block alloca?
            self.builder.build_store(alloca, value).unwrap();
            self.symbol_table
                .add_variable(name, Variable::Mutable(alloca));
        } else {
            self.symbol_table
                .add_variable(name, Variable::Immutable(value));
        }
    }

    // Generates LLVM IR for return expressions.
    // fn generate_return(&mut self, expr_opt: &Option<Expr>) {
    //     let return_value = expr_opt.as_ref().map(|expr| {
    //         let val = self.generate_expression(expr).unwrap();
    //         Box::new(val) as Box<dyn BasicValue>
    //     });
    //
    //     self.builder.build_return(return_value.as_deref()).unwrap();
    // }
}
