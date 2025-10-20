use crate::codegen::value::Value;
use crate::codegen::CodeGen;
use crate::hir::{Expression, Type};
use inkwell::types::BasicType;
use inkwell::values::{BasicValue, BasicValueEnum};

impl<'ctx> CodeGen<'ctx> {
    /// Generates LLVM IR for struct literals.
    ///
    /// The generated value is always an R-Value, as the result of the function
    /// is a value that can be used in other expressions. It should be treated the same as
    /// a literal, but is kept separately from the other literals in the AST.
    pub(super) fn generate_struct_literal_r_value(
        &mut self,
        struct_name: &str,
        struct_type: &Type,
        fields: &[(String, Expression)],
    ) -> Value<'ctx> {
        // 1) Get the struct type from the struct definition table.
        let llvm_struct_type = self
            .struct_definition_table
            .get(struct_name)
            .unwrap()
            .0
            .as_basic_type_enum();
        
        let mut zero = llvm_struct_type.const_zero().as_basic_value_enum();
        for (i, (_, field_expr)) in fields.iter().enumerate() {
            let field_value =
                BasicValueEnum::from(self.generate_expression_r_value(field_expr).unwrap());

            zero = self
                .builder
                .build_insert_value(
                    zero.into_struct_value(),
                    field_value,
                    i as u32,
                    &format!("insert_{}_{}", struct_name, i),
                )
                .unwrap()
                .as_basic_value_enum();
        }

        Value::new(self.to_basic_type_enum(struct_type).unwrap(), zero)
    }
}
