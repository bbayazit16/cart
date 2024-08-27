use crate::ast;
use crate::token::TokenType;
use inkwell::context::Context;
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
};

impl ast::Type {
    /// Converts the type to an LLVM BasicTypeEnum. Returns None if the type is void, ().
    pub(super) fn to_basic_type_enum<'ctx>(
        &self,
        ctx: &'ctx Context,
    ) -> Option<BasicTypeEnum<'ctx>> {
        match self {
            ast::Type::Simple(tok) => match &tok.token_type {
                TokenType::Identifier(s) => Some(match s.as_str() {
                    "i32" => ctx.i32_type().as_basic_type_enum(),
                    "i64" => ctx.i64_type().as_basic_type_enum(),
                    "f32" => ctx.f32_type().as_basic_type_enum(),
                    "f64" => ctx.f64_type().as_basic_type_enum(),
                    _ => todo!(),
                }),
                _ => todo!(),
            },
            ast::Type::Empty(_) => None,
            _ => todo!(),
        }
    }

    // Converts the type ot an LLVM AnyTypeEnum.
    pub(super) fn to_any_type_enum<'ctx>(&self, ctx: &'ctx Context) -> AnyTypeEnum<'ctx> {
        self.to_basic_type_enum(ctx)
            .map(|ty| ty.as_any_type_enum())
            .unwrap_or(ctx.void_type().as_any_type_enum())
    }
}

/// Creates a function type from the given return type and parameter types.
pub(super) fn create_function_type<'ctx>(
    context: &'ctx Context,
    return_type: AnyTypeEnum<'ctx>,
    param_types: Vec<BasicTypeEnum<'ctx>>,
) -> FunctionType<'ctx> {
    let return_type = match return_type {
        AnyTypeEnum::VoidType(_) => None,
        AnyTypeEnum::IntType(ty) => Some(BasicTypeEnum::IntType(ty)),
        AnyTypeEnum::FloatType(ty) => Some(BasicTypeEnum::FloatType(ty)),
        _ => todo!("Other return types"),
    };

    let param_types: Vec<BasicMetadataTypeEnum> = param_types
        .into_iter()
        .map(BasicMetadataTypeEnum::from)
        .collect();

    match return_type {
        Some(ty) => ty.fn_type(&param_types, false),
        None => context.void_type().fn_type(&param_types, false),
    }
}
