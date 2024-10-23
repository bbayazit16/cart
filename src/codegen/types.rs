use crate::codegen::value::Value;
use crate::codegen::CodeGen;
use crate::hir::Type;
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
};
use inkwell::AddressSpace;

impl<'ctx> CodeGen<'ctx> {
    /// Converts the type to Inkwell AnyTypeEnum.
    pub(super) fn to_any_type_enum(&self, ty: &Type) -> AnyTypeEnum<'ctx> {
        match ty {
            Type::Int => self.context.i32_type().as_any_type_enum(),
            Type::Int64 => self.context.i64_type().as_any_type_enum(),
            Type::Int128 => self.context.i128_type().as_any_type_enum(),
            Type::Int256 => self.context.custom_width_int_type(256).as_any_type_enum(),
            Type::Float => self.context.f32_type().as_any_type_enum(),
            Type::Float64 => self.context.f64_type().as_any_type_enum(),
            Type::Bool => self.context.bool_type().as_any_type_enum(),
            Type::String => self
                .context
                .struct_type(
                    &[
                        self.context.i64_type().into(), // Reference count
                        self.context.i64_type().into(), // Length
                        self.context.ptr_type(AddressSpace::default()).into(), // Pointer to string data
                    ],
                    false,
                )
                .as_any_type_enum(),
            Type::Unit => self.context.void_type().as_any_type_enum(),
            Type::Array(_) => todo!(),
            Type::Struct(struct_name) => self
                .struct_definition_table
                .get(struct_name)
                .unwrap()
                .0
                .as_any_type_enum(),
            Type::Enum(_) => todo!(),
            Type::Error(_) => todo!(),
            Type::Generic(_, _) => todo!(),
            Type::DeclaredGeneric(_) => todo!(),
        }
    }

    /// Converts the type to Inkwell BasicTypeEnum.
    pub(super) fn to_basic_type_enum(&self, ty: &Type) -> Option<BasicTypeEnum<'ctx>> {
        let any_type_enum = self.to_any_type_enum(ty);
        BasicTypeEnum::try_from(any_type_enum).ok()
    }

    /// Returns the index of the field in the struct.
    pub(super) fn struct_field_index(&self, struct_name: &str, struct_field: &str) -> usize {
        self.struct_definition_table
            .get(struct_name)
            .unwrap()
            .1
            .get(struct_field)
            .unwrap()
            .0
    }

    /// Creates a function type from the given return type and parameter types.
    pub(super) fn create_function_type(
        &self,
        return_type: &Type,
        param_types: &[&Type],
    ) -> FunctionType<'ctx> {
        let return_type = self.to_basic_type_enum(return_type);
        let param_types: Vec<BasicMetadataTypeEnum> = param_types
            .iter()
            // Why unwrapping is safe: The non-void types are already verified by the HIR.
            .map(|ty| self.to_basic_type_enum(ty).unwrap().into())
            .collect();

        match return_type {
            Some(non_void) => non_void.fn_type(&param_types, false),
            None => self.context.void_type().fn_type(&param_types, false),
        }
    }

    /// Set the value as an r-value. If already an r-value, do nothing.
    pub(super) fn as_r_value(&mut self, value: &mut Value<'ctx>) {
        if value.is_l_value {
            let loaded_var = self
                .builder
                .build_load(
                    value.type_enum,
                    value.basic_value.into_pointer_value(),
                    "cast_l_value",
                )
                .unwrap();

            value.basic_value = loaded_var;
            value.is_l_value = false;
        }
    }
}
