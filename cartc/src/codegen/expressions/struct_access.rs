use crate::codegen::value::{LValue, Value, ValueState};
use crate::codegen::CodeGen;
use crate::hir::{Expression, Type};
use inkwell::types::BasicTypeEnum;
use inkwell::values::PointerValue;
use inkwell::values::{BasicValue, BasicValueEnum};

impl<'ctx> CodeGen<'ctx> {
    /// Generates LLVM IR for struct access expressions.
    ///
    /// Struct access always returns an L-Value, as it is likely to be used for assignment.
    /// If not used for assignments, then the value can always be cast to an R-Value.
    ///
    /// Steps to generate a struct access expression:
    ///     1) Generate the object expression. This must always be a pointer to the struct.
    ///     2) Get the field index from the struct definition table.
    ///     3) Generate a GEP instruction to get the field.
    ///     4) Return the GEP pointer as an L-Value.
    pub(super) fn generate_struct_access(
        &mut self,
        object: &Expression,
        object_name: &str,
        field: &str,
        returned_field_type: &Type,
    ) -> Value<'ctx, LValue> {
        // 1) Generate the object expression. This must always be a pointer to the struct.
        let struct_object = self.generate_expression(object).unwrap();
        let struct_object_ptr = match struct_object {
            ValueState::L(l_value) => PointerValue::from(l_value),
            ValueState::R(r_value) => {
                // If the object is an R-Value, then r_value is a struct value.
                // We need to create an alloca, store the value, and then get the pointer.
                let alloca = self.create_entry_block_alloca(
                    BasicTypeEnum::from(r_value),
                    format!("alloca_struct_{}", object_name).as_str(),
                );

                // Store the struct_object in the alloca.
                self.builder
                    .build_store(alloca, BasicValueEnum::from(r_value))
                    .unwrap();

                alloca
            }
        };

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
        Value::new_l(
            self.to_basic_type_enum(returned_field_type).unwrap(),
            gep.as_basic_value_enum(),
        )
    }
}
