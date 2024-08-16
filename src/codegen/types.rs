//! Types module converts the Type AST
//! into LLVM types.
//!
use crate::ast::Type;
use crate::token::Token;
use inkwell::context::Context;
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
};

impl Type {
    pub(crate) fn to_llvm_type<'ctx>(&self, ctx: &'ctx Context) -> AnyTypeEnum<'ctx> {
        match self {
            Type::Simple(tok) => match tok {
                Token::Identifier(_, s) => match s.as_str() {
                    "i32" => ctx.i32_type().as_any_type_enum(),
                    "i64" => ctx.i64_type().as_any_type_enum(),
                    "f32" => ctx.f32_type().as_any_type_enum(),
                    "f64" => ctx.f64_type().as_any_type_enum(),
                    _ => todo!(),
                },
                _ => todo!(),
            },
            Type::Empty => ctx.void_type().as_any_type_enum(),
            _ => todo!(),
        }
    }
}

/// Creates a function type from the given return type and parameter types.
pub(crate) fn create_function_type<'ctx>(
    context: &'ctx Context,
    return_type: AnyTypeEnum<'ctx>,
    param_types: Vec<AnyTypeEnum<'ctx>>,
) -> FunctionType<'ctx> {
    let return_type = match return_type {
        AnyTypeEnum::VoidType(_) => None,
        AnyTypeEnum::IntType(ty) => Some(BasicTypeEnum::IntType(ty)),
        AnyTypeEnum::FloatType(ty) => Some(BasicTypeEnum::FloatType(ty)),
        _ => todo!("Other return types"),
    };

    let param_types: Vec<BasicMetadataTypeEnum> = param_types
        .into_iter()
        .map(|ty| match ty {
            AnyTypeEnum::IntType(ty) => BasicMetadataTypeEnum::IntType(ty),
            AnyTypeEnum::FloatType(ty) => BasicMetadataTypeEnum::FloatType(ty),
            _ => todo!("Other parameter types"),
        })
        .collect();

    match return_type {
        Some(ty) => ty.fn_type(&param_types, false),
        None => context.void_type().fn_type(&param_types, false),
    }
}
