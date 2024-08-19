use crate::ast::{LetStmt, Stmt};
use crate::codegen::symbol_table::Variable;
use crate::codegen::CodeGen;
use crate::token_value;
use inkwell::types::BasicTypeEnum;

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
        let (value_type, value) = self.generate_expression(&let_stmt.expr).unwrap();
        let name = token_value!(&let_stmt.name);

        let alloca;
        if value_type.is_alloca {
            alloca = value.into_pointer_value();
        } else {
            let basic_type_enum: BasicTypeEnum = value_type.clone().into();
            alloca = self.create_entry_block_alloca(basic_type_enum, &name);
            self.builder.build_store(alloca, value).unwrap();
        }

        let variable = if let_stmt.is_mut {
            Variable::Mutable(value_type, alloca)
        } else {
            Variable::Immutable(value_type, alloca)
        };

        self.symbol_table.add_variable(name, variable);
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
