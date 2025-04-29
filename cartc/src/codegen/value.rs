use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, PointerValue};

/// `Value` wraps around Inkwell `BasicTypeEnum` and allows extra information
/// to be assigned to the types.
///
/// Regardless of whether a value is an R-Value or an L-Value, the `type_enum` field
/// always stores the type of the original value. So, an integer that is an L-Value will
/// have an integer type stored in `type_enum`, and NOT a pointer type.
///
/// Users must be mindful of this distinction when using the `Value` struct.
#[derive(Debug, Clone, Copy)]
pub(super) struct Value<'ctx> {
    type_enum: BasicTypeEnum<'ctx>,
    basic_value: BasicValueEnum<'ctx>,
}

impl<'ctx> Value<'ctx> {
    // Create a new `Value`.
    pub(super) fn new(type_enum: BasicTypeEnum<'ctx>, basic_value: BasicValueEnum<'ctx>) -> Self {
        Self {
            type_enum,
            basic_value,
        }
    }
}

impl<'ctx> From<Value<'ctx>> for BasicValueEnum<'ctx> {
    fn from(value: Value<'ctx>) -> Self {
        value.basic_value
    }
}

impl<'ctx> From<Value<'ctx>> for BasicTypeEnum<'ctx> {
    fn from(value: Value<'ctx>) -> Self {
        value.type_enum
    }
}
