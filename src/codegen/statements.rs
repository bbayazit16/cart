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
            Stmt::Let(ref let_stmt) => {
                self.generate_let_stmt(let_stmt);
            },
            Stmt::Return(_) => unreachable!("Return statement"),
            _ => todo!(),
        }
    }

    /// Generates LLVM IR for let statements.
    fn generate_let_stmt(&mut self, let_stmt: &LetStmt) {
        let (value_type, value) = self.generate_expression(&let_stmt.expr).unwrap();
        let name = token_value!(&let_stmt.name);

        let alloca;
        if value_type.is_alloca {
            alloca = value.into_pointer_value();
        } else {
            alloca = self.create_entry_block_alloca(value_type.type_enum, &name);
            self.builder.build_store(alloca, value).unwrap();
        }

        let variable = if let_stmt.is_mut {
            Variable::Mutable(value_type, alloca)
        } else {
            Variable::Immutable(value_type, alloca)
        };

        self.symbol_table.add_variable(name, variable);
    }
}
