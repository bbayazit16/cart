use inkwell::values::BasicValue;
use crate::codegen::CodeGen;
use crate::codegen::value::Value;
use crate::hir::{Expression, Statement, Type};

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
            },
            // Stmt::Return(_) => unreachable!("Return statement"),
            _ => todo!(),
        }
    }

    /// Generates LLVM IR for let statements.
    fn generate_let_stmt(&mut self, name: &String, ty: &Type, value: &Expression) {
        // Unwrapping below is safe, as type checker ensures that unit types are not assigned.
        let r_value = self.generate_expression(value).unwrap();
        
        // Why unwrapping is safe: We know a unit type isn't being assigned.
        let llvm_type = ty.to_basic_type_enum(self.context).unwrap();

        let alloca = self.create_entry_block_alloca(
            llvm_type,
            &format!("alloca_var_{}", name),
        );

        self.builder
            .build_store(alloca, r_value.basic_value)
            .unwrap();

        self.symbol_table.add(name.clone(), Value::new(
            llvm_type,
            alloca.as_basic_value_enum()
        ));

        // let (value_type, value) = self.generate_expression(&let_stmt.expr).unwrap();
        // let name = token_value!(&let_stmt.name);
        //
        // let alloca;
        // if value_type.is_alloca {
        //     alloca = value.into_pointer_value();
        // } else {
        //     alloca = self.create_entry_block_alloca(value_type.type_enum, &name);
        //     self.builder.build_store(alloca, value).unwrap();
        // }
        //
        // let variable = if let_stmt.is_mut {
        //     Variable::Mutable(value_type, alloca)
        // } else {
        //     Variable::Immutable(value_type, alloca)
        // };
        //
        // self.symbol_table.add(name, variable);
    }
}
