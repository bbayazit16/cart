use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum;

/// `Value` wraps around Inkwell `BasicTypeEnum` and allows extra information
/// to be assigned to the types. It employs a builder-like pattern.
#[derive(Debug, Clone, Copy)]
pub(super) struct Value<'ctx> {
    pub type_enum: BasicTypeEnum<'ctx>,
    pub basic_value: BasicValueEnum<'ctx>,
    pub is_l_value: bool,
}

impl<'ctx> Value<'ctx> {
    /// Create a new value with type_enum and value.
    pub(super) fn new(type_enum: BasicTypeEnum<'ctx>, basic_value: BasicValueEnum<'ctx>) -> Self {
        Self {
            type_enum,
            basic_value,
            is_l_value: false,
        }
    }

    /// Set the value as an l-value.
    pub(super) fn as_l_value(mut self) -> Self {
        self.is_l_value = true;
        self
    }
}
