use crate::codegen::value::Value;
use crate::codegen::CodeGen;
use crate::hir::{Expression, Type};
use inkwell::types::BasicType;
use inkwell::values::{BasicValueEnum};

impl<'ctx> CodeGen<'ctx> {
    /// Generates LLVM IR for struct literals.
    ///
    /// The generated value is always an R-Value, as the result of the function
    /// is a value that can be used in other expressions. It should be treated the same as
    /// a literal, but is kept separately from the other literals in the AST.
    ///
    /// Steps to generate a struct literal are as follows:
    ///     1) Get the struct type from the struct definition table.
    ///        - At this point, type checker ensures the struct is defined.
    ///    2) Create an alloca for the struct literal.
    ///    3) Generate the expressions for each one of the struct fields.
    ///        - 3.1) If the field is an L-Value, load it.
    ///    4) Load the struct literal.
    ///    5) Return the loaded struct literal as an R-Value.
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

        // 2) Create an alloca for the struct literal.
        let struct_ptr = self.create_entry_block_alloca(
            llvm_struct_type,
            format!("struct_literal_ptr_{}", struct_name).as_str(),
        );

        // 3) Generate the expressions for each one of the struct fields.
        for (field_name, field_expr) in fields {
            // 3.1) If the field is an L-Value, load it.
            let llvm_field_expr = self.generate_expression_r_value(field_expr).unwrap();

            // Get the field type from the struct definition table.
            let gep = self
                .builder
                .build_struct_gep(
                    llvm_struct_type,
                    struct_ptr,
                    self.struct_field_index(struct_name, field_name) as u32,
                    field_name,
                )
                .unwrap();

            // Store the field value in the struct.
            self.builder
                .build_store(gep, BasicValueEnum::from(llvm_field_expr))
                .unwrap();
        }

        // 4) Load the struct literal.
        let loaded = self
            .builder
            .build_load(
                llvm_struct_type,
                struct_ptr,
                format!("loaded_struct_{}", struct_name).as_str(),
            )
            .unwrap();

        // 5) Return the loaded struct literal as an R-Value.
        Value::new(self.to_basic_type_enum(struct_type).unwrap(), loaded)
    }
}
