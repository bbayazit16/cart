use crate::codegen::value::Value;
use crate::codegen::CodeGen;
use crate::hir::{Expression, Statement, Type};
use inkwell::values::BasicValue;

impl<'ctx> CodeGen<'ctx> {
    // /// Generates the LLVM IR for a statement.
    pub(super) fn generate_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Expression(ref expr) => {
                self.generate_expression(expr);
            }
            Statement::Let {
                ref name,
                ref ty,
                ref value,
            } => {
                self.generate_let_stmt(name, ty, value);
            }
        }
    }

    /// Generates LLVM IR for let statements.
    fn generate_let_stmt(&mut self, name: &String, ty: &Type, value: &Expression) {
        // Unwrapping below is safe, as type checker ensures that unit types are not assigned.
        let mut r_value = self.generate_expression(value).unwrap();
        self.as_l_value(&mut r_value);

        let llvm_type = r_value.type_enum;

        let alloca = self.create_entry_block_alloca(llvm_type, &format!("alloca_var_{}", name));

        self.builder
            .build_store(alloca, r_value.basic_value)
            .unwrap();

        self.symbol_table.add(
            name.clone(),
            Value::new(llvm_type, alloca.as_basic_value_enum()),
        );
    }
}
