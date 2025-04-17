use crate::codegen::value::{Value, ValueState};
use crate::codegen::CodeGen;
use crate::hir::Expression;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum};

impl CodeGen<'_> {
    /// Generates LLVM IR for let statements.
    ///
    /// The steps to generate a let statement are as follows:
    ///    1) Generate the value of the expression.
    ///    2) Create an alloca for the variable.
    ///    3) Store the value in the alloca.
    ///    4) Add the variable to the symbol table.
    pub(super) fn generate_let_stmt(&mut self, name: &String, value: &Expression) {
        // 1) Generate the value of the expression.
        // Unwrapping below is safe, as type checker ensures that unit types are not assigned.
        let r_value = self.generate_expression(value).unwrap();
        assert!(matches!(r_value, ValueState::R(_)));
        let r_value = match r_value {
            ValueState::R(r) => r,
            _ => unreachable!(),
        };

        // 2) Create an alloca for the variable.
        let alloca = self.create_entry_block_alloca(
            BasicTypeEnum::from(r_value),
            &format!("alloca_var_{}", name),
        );

        // 3) Store the value in the alloca.
        self.builder
            .build_store(alloca, BasicValueEnum::from(r_value))
            .unwrap();

        // 4) Add the variable to the symbol table.
        self.symbol_table.add(
            name.clone(),
            Value::new_l(BasicTypeEnum::from(r_value), alloca.as_basic_value_enum()),
        );
    }
}
