use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum;

/// `Value` wraps around Inkwell `BasicTypeEnum` and allows extra information
/// to be assigned to the types. It employs a builder-like pattern.
#[derive(Debug, Clone, Copy)]
pub(super) struct Value<'ctx> {
    pub type_enum: BasicTypeEnum<'ctx>,
    pub basic_value: BasicValueEnum<'ctx>,
    pub is_alloca: bool,
}

impl<'ctx> Value<'ctx> {
    /// Create a new value with type_enum and value.
    pub(super) fn new(type_enum: BasicTypeEnum<'ctx>, basic_value: BasicValueEnum<'ctx>) -> Self {
        Self {
            type_enum,
            basic_value,
            is_alloca: false,
        }
    }
}

// pub(super) struct ValueBuilder<'ctx> {
//     pub type_enum: Option<BasicTypeEnum<'ctx>>,
//     pub value: Option<BasicValueEnum<'ctx>>,
//     pub is_alloca: Option<bool>,
// }
//
// impl<'ctx> ValueBuilder<'ctx> {
//     pub fn new() -> Self {
//         Self {
//             type_enum: None,
//             value: None,
//             is_alloca: None,
//         }
//     }
//
//     pub fn with_type(mut self, type_enum: BasicTypeEnum<'ctx>) -> Self {
//         self.type_enum = Some(type_enum);
//         self
//     }
//
//     pub fn with_value(mut self, value: BasicValueEnum<'ctx>) -> Self {
//         self.value = Some(value);
//         self
//     }
//
//     pub fn with_alloca(mut self, is_alloca: bool) -> Self {
//         self.is_alloca = Some(is_alloca);
//         self
//     }
//
//     pub fn build(self) -> Value<'ctx> {
//         Value {
//             type_enum: self.type_enum.expect("Type not set"),
//             value: self.value.expect("Value not set"),
//             is_alloca: self.is_alloca.unwrap_or(false)
//         }
//     }
// }
