use crate::codegen::value::Value;
use crate::codegen::CodeGen;
use crate::hir::{Expression, Type};
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum};

impl<'ctx> CodeGen<'ctx> {
    /// Generates LLVM IR for struct access expressions, for l-values only.
    ///
    /// Steps to generate a struct access expression:
    ///     1) Generate the object expression. This must always be a pointer to the struct.
    ///     2) Get the field index from the struct definition table.
    ///     3) Generate a GEP instruction to get the field.
    ///     4) Return the GEP pointer as an L-Value.
    pub(super) fn generate_struct_access_l_value(
        &mut self,
        object: &Expression,
        object_name: &str,
        field: &str,
        returned_field_type: &Type,
    ) -> Value<'ctx> {
        // 1) Generate the object expression. This must always be a pointer to the struct.
        // Note: DO NOT generate an l_value directly here. Consider an example such as:
        // Point { x: 4, y: 4 }.x
        // Then, trying to generate an l-value for the struct literal would be invalid.
        // So we must always generate an r-value and create a pointer out of it.
        let struct_object_ptr = {
            let struct_object = self.generate_expression_r_value(object).unwrap();
            if BasicTypeEnum::from(struct_object).is_pointer_type() {
                BasicValueEnum::from(struct_object).into_pointer_value()
            } else {
                let alloca = self.create_entry_block_alloca(
                    BasicTypeEnum::from(struct_object),
                    format!("alloca_struct_{}", object_name).as_str(),
                );
                // Store the struct_object in the alloca.
                self.builder
                    .build_store(alloca, BasicValueEnum::from(struct_object))
                    .unwrap();

                alloca
            }
        };
        // let struct_object = self.generate_expression_r_value(object).unwrap();

        // 2) Get the field index from the struct definition table.
        // At this point, we know that the struct definition exists, verified in the type checker.
        let struct_type = self.struct_definition_table.get(object_name).unwrap().0;
        let field_index = self.struct_field_index(object_name, field);

        // 3) Generate a GEP instruction to get the field.
        let gep = self
            .builder
            .build_struct_gep(struct_type, struct_object_ptr, field_index as u32, field)
            .unwrap();

        // TODO: Support multiple fields

        // 4) Return the GEP pointer as an L-Value.
        Value::new(
            self.to_basic_type_enum(returned_field_type).unwrap(),
            gep.as_basic_value_enum(),
        )
    }

    /// Generate a struct access expression, resulting in an r-value.
    /// Method is the same as `generate_struct_access_r_value` defined above.
    pub(super) fn generate_struct_access_r_value(
        &mut self,
        object: &Expression,
        object_name: &str,
        field: &str,
        returned_field_type: &Type,
    ) -> Value<'ctx> {
        let value =
            self.generate_struct_access_l_value(object, object_name, field, returned_field_type);

        let loaded_r_value = self
            .builder
            .build_load(
                BasicTypeEnum::from(value),
                BasicValueEnum::from(value).into_pointer_value(),
                &format!("load_cast_{}", object_name),
            )
            .unwrap();

        Value::new(BasicTypeEnum::from(value), loaded_r_value)
    }
}
