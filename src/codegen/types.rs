use crate::hir::Type;
use inkwell::context::Context;
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
};

/// Creates a function type from the given return type and parameter types.
pub(super) fn create_function_type<'ctx>(
    context: &'ctx Context,
    return_type: &Type,
    param_types: &[&Type],
) -> FunctionType<'ctx> {
    let return_type = return_type.to_basic_type_enum(context);
    let param_types: Vec<BasicMetadataTypeEnum> = param_types
        .iter()
        // Why unwrapping is safe: The non-void types are already verified by the HIR.
        .map(|ty| ty.to_basic_type_enum(context).unwrap().into())
        .collect();

    match return_type {
        Some(non_void) => non_void.fn_type(&param_types, false),
        None => context.void_type().fn_type(&param_types, false),
    }
}

impl Type {
    /// Converts the type to Inkwell AnyTypeEnum.
    pub(super) fn to_any_type_enum<'ctx>(&self, ctx: &'ctx Context) -> AnyTypeEnum<'ctx> {
        match self {
            Type::Int => ctx.i32_type().as_any_type_enum(),
            Type::Int64 => ctx.i64_type().as_any_type_enum(),
            Type::Int128 => ctx.i128_type().as_any_type_enum(),
            Type::Int256 => ctx.custom_width_int_type(256).as_any_type_enum(),
            Type::Float => ctx.f32_type().as_any_type_enum(),
            Type::Float64 => ctx.f64_type().as_any_type_enum(),
            Type::Bool => ctx.bool_type().as_any_type_enum(),
            Type::String => todo!(),
            Type::Unit => ctx.void_type().as_any_type_enum(),
            Type::Array(_) => todo!(),
            Type::Struct(_) => todo!(),
            Type::Enum(_) => todo!(),
            Type::Error(_) => todo!(),
            Type::Generic(_, _) => todo!(),
            Type::DeclaredGeneric(_) => todo!(),
        }
    }

    /// Converts the type to Inkwell BasicTypeEnum.
    pub(super) fn to_basic_type_enum<'ctx>(
        &self,
        ctx: &'ctx Context,
    ) -> Option<BasicTypeEnum<'ctx>> {
        let any_type_enum = self.to_any_type_enum(ctx);
        BasicTypeEnum::try_from(any_type_enum).ok()
    }
}
