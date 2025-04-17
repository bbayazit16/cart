use crate::codegen::value::{RValue, Value, ValueState};
use crate::codegen::CodeGen;
use crate::hir::{Expression, Type};
use inkwell::values::{BasicValueEnum, PointerValue};

impl<'ctx> CodeGen<'ctx> {
    /// Generates LLVM IR for assignment expressions.
    ///
    /// Assignment expressions always return an R-Value, the value that was just assigned.
    ///
    /// Steps to generate an assignment expression:
    ///    1) Generate the right-hand side expression and cast to R-Value if necessary.
    ///    2) Generate the left-hand side of the expression (which must be an L-Value).
    ///    3) Store the value in the left-hand side.
    ///    4) Return the value that was just assigned.
    pub(super) fn generate_assignment(
        &mut self,
        l_value: &Expression,
        l_value_type: &Type,
        r_value: &Expression,
        r_value_type: &Type,
    ) -> Value<'ctx, RValue> {
        // 1) Generate the right-hand side expression and cast to R-Value if necessary.
        let expr = self.generate_expression(r_value).unwrap();
        let r_value = self.cast_to_r_value(expr);

        // 2) Generate the left-hand side of the expression (which must be an L-Value).
        let l_value = self.generate_expression(l_value).unwrap();
        // Assert that it is an L-Value.
        // assert!(matches!(l_value, ValueState::L(_)));

        let l_value_ptr = match l_value {
            ValueState::L(l_value) => PointerValue::from(l_value),
            _ => unreachable!(),
        };

        // let bve = if r_value_type.is_reference() {
        //     let alloca = self.create_entry_block_alloca(
        //         self.to_basic_type_enum(r_value_type).unwrap(),
        //         "alloca_assignment_ref",
        //     );
        //     self.builder
        //         .build_store(alloca, BasicValueEnum::from(r_value))
        //         .unwrap();
        //     alloca.into()
        // } else {
        //     BasicValueEnum::from(r_value)
        // };

        // 3) Store the value in the left-hand side.
        self.builder
            .build_store(l_value_ptr, BasicValueEnum::from(r_value))
            .unwrap();

        // 4) Return the value that was just assigned.
        Value::new_r(
            self.to_basic_type_enum(l_value_type).unwrap(),
            BasicValueEnum::from(r_value),
        )
    }
}
